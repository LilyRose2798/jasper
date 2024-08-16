import gleam/float
import gleam/int
import gleam/string
import gleam/result
import gleam/dict.{type Dict}
import gleam/list
import jasper/internal/parsing.{
  type ParseError, type Parser, Parser, Position, Unexpected, between, char,
  choice, choice_char, concat, digit, do, either, end, go, lazy, left, letter,
  many, many1_concat, many_concat, map, pair, perhaps, perhaps_default,
  perhaps_empty, return, right, satisfy, sep, string, to,
}

pub type JsonParseError =
  Nil

pub type JsonParserError =
  ParseError(JsonParseError)

pub type JsonParser(a) =
  Parser(a, JsonParseError)

pub type JsonObject =
  Dict(String, JsonValue)

pub type JsonArray =
  List(JsonValue)

pub type JsonValue {
  Object(JsonObject)
  Array(JsonArray)
  String(String)
  Number(Float)
  Boolean(Bool)
  Null
}

fn ws(options: JsonParseOptions) -> JsonParser(String) {
  many_concat(
    satisfy(fn(c) {
      string.contains(
        case options.ecma_whitespace {
          True ->
            " \n\r\t\u{000B}\u{000C}\u{00A0}\u{2028}\u{2029}\u{FEFF}\u{1680}\u{2000}\u{2001}\u{2002}\u{2003}\u{2004}\u{2005}\u{2006}\u{2007}\u{2008}\u{2009}\u{200A}\u{202F}\u{205F}\u{3000}"
          False -> " \n\r\t"
        },
        c,
      )
    }),
  )
}

fn padded(p: JsonParser(a), options: JsonParseOptions) -> JsonParser(a) {
  left(p, ws(options))
}

fn symbol(s: String, options: JsonParseOptions) -> JsonParser(String) {
  char(s)
  |> padded(options)
}

fn json_null_parser() -> JsonParser(Nil) {
  string("null")
  |> to(Nil)
}

fn json_boolean_parser() -> JsonParser(Bool) {
  either(to(string("true"), True), to(string("false"), False))
}

fn hex_digit_parser() -> JsonParser(String) {
  choice_char([
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e",
    "f", "A", "B", "C", "D", "E", "F",
  ])
}

fn json_number_parser(options: JsonParseOptions) -> JsonParser(Float) {
  let sign_parser =
    either(char("+"), char("-"))
    |> perhaps_empty
  let int_parser =
    either(
      char("0"),
      concat(
        choice_char(["1", "2", "3", "4", "5", "6", "7", "8", "9"]),
        many_concat(digit()),
      ),
    )
  let exp_parser =
    either(char("e"), char("E"))
    |> concat(sign_parser)
    |> concat(many1_concat(digit()))
    |> perhaps_empty
  let min_double = -1.7976931348623158e308
  let max_double = 1.7976931348623158e308

  case options.ecma_numbers {
    True -> {
      use sign <- do(sign_parser)
      let inf = case sign {
        "-" -> min_double
        _ -> max_double
      }
      choice([
        to(string("Infinity"), inf),
        to(string("NaN"), 0.0),
        {
          use hex <- do(
            either(string("0x"), string("0X"))
            |> right(many1_concat(hex_digit_parser())),
          )
          let assert Ok(n) = int.base_parse(sign <> hex, 16)
          return(int.to_float(n))
        },
        {
          use n <- do(either(
            {
              use ns <- do(int_parser)
              use ds <- do(
                right(
                  char("."),
                  many1_concat(digit())
                    |> perhaps_default("0"),
                )
                |> perhaps_default("0"),
              )
              return(ns <> "." <> ds)
            },
            concat(to(char("."), "0."), many1_concat(digit())),
          ))
          use ex <- do(exp_parser)
          { sign <> n <> ex }
          |> float.parse
          |> result.unwrap(inf)
          |> return
        },
      ])
    }
    False -> {
      use sign <- do(
        char("-")
        |> perhaps_empty,
      )
      let inf = case sign {
        "-" -> min_double
        _ -> max_double
      }
      use ns <- do(int_parser)
      use ds <- do(
        right(char("."), many1_concat(digit()))
        |> perhaps_default("0"),
      )
      use ex <- do(exp_parser)
      { sign <> ns <> "." <> ds <> ex }
      |> float.parse
      |> result.unwrap(inf)
      |> return
    }
  }
}

fn string_to_codepoint(value: String) -> String {
  let assert Ok(number) = int.base_parse(value, 16)
  let assert Ok(codepoint) = string.utf_codepoint(number)
  string.from_utf_codepoints([codepoint])
}

fn valid_string_char(c: String) -> Bool {
  let assert [p] = string.to_utf_codepoints(c)
  let i = string.utf_codepoint_to_int(p)
  i >= 0x20 && i <= 0x10FFFF
}

fn json_string_parser(options: JsonParseOptions) -> JsonParser(String) {
  let hex_digit = hex_digit_parser()
  let unicode_escape =
    map(
      right(
        char("u"),
        hex_digit
          |> concat(hex_digit)
          |> concat(hex_digit)
          |> concat(hex_digit),
      ),
      string_to_codepoint,
    )

  let escape =
    char("\\")
    |> right(
      choice(case options.ecma_strings {
        True -> [
          map(
            right(char("x"), concat(hex_digit, hex_digit)),
            string_to_codepoint,
          ),
          unicode_escape,
          char("\\"),
          char("/"),
          char("'"),
          char("\""),
          to(char("b"), "\u{0008}"),
          to(char("f"), "\u{000C}"),
          to(char("n"), "\n"),
          to(char("r"), "\r"),
          to(char("t"), "\t"),
          to(char("v"), "\u{000B}"),
          to(char("0"), "\u{0000}"),
          to(
            choice([
              string("\r\n"),
              char("\r"),
              char("\n"),
              char("\u{2028}"),
              char("\u{2029}"),
            ]),
            "",
          ),
          satisfy(fn(c) { !string.contains("123456789", c) }),
        ]
        False -> [
          unicode_escape,
          char("\\"),
          char("/"),
          char("\""),
          to(char("b"), "\u{0008}"),
          to(char("f"), "\u{000C}"),
          to(char("n"), "\n"),
          to(char("r"), "\r"),
          to(char("t"), "\t"),
        ]
      }),
    )

  let str = fn(q) {
    satisfy(fn(c) { c != q && c != "\\" && valid_string_char(c) })
    |> either(escape)
    |> many_concat()
    |> between(char(q), char(q))
  }

  case options.ecma_strings {
    True -> either(str("\""), str("'"))
    False -> str("\"")
  }
}

fn parse_multiline_comment() -> JsonParser(String) {
  Parser(fn(source, pos) {
    let assert Position(row, col) = pos
    case source {
      [h, ..t] ->
        case h {
          "\n" -> Ok(#(h, t, Position(row + 1, 0)))
          "*" ->
            case t {
              ["/", ..] -> Error(Unexpected(pos, h))
              _ -> Ok(#(h, t, Position(row, col + 1)))
            }
          _ -> Ok(#(h, t, Position(row, col + 1)))
        }
      [] -> Error(Unexpected(pos, "EOF"))
    }
  })
  |> many_concat
  |> between(string("/*"), string("*/"))
}

fn parse_singleline_comment() -> JsonParser(String) {
  right(
    string("//"),
    many_concat(satisfy(fn(c) { !string.contains("\r\n\u{2028}\u{2029}", c) })),
  )
}

fn comment_parser(options: JsonParseOptions) -> JsonParser(String) {
  either(parse_multiline_comment(), parse_singleline_comment())
  |> padded(options)
}

fn allow_comments(options: JsonParseOptions, p: Parser(_, _)) {
  case options.comments {
    True -> left(p, many(comment_parser(options)))
    False -> p
  }
}

fn allow_trailing_comma(options: JsonParseOptions, p: Parser(_, _)) {
  case options.trailing_comma {
    True -> right(perhaps(symbol(",", options)), p)
    False -> p
  }
}

fn json_array_parser(options: JsonParseOptions) -> JsonParser(JsonArray) {
  let json_value_parser = lazy(fn() { json_value_parser(options) })
  let allow_comments = allow_comments(options, _)
  sep(json_value_parser, allow_comments(symbol(",", options)))
  |> between(
    allow_comments(symbol("[", options)),
    allow_trailing_comma(options, symbol("]", options)),
  )
}

// todo
fn ecmascript_identifier_parser() -> JsonParser(String) {
  letter()
  |> concat(
    either(letter(), digit())
    |> many_concat,
  )
}

fn json_object_parser(options: JsonParseOptions) -> JsonParser(JsonObject) {
  let json_value_parser = lazy(fn() { json_value_parser(options) })
  let allow_comments = allow_comments(options, _)
  let key_parser = case options.ecma_object_keys {
    True -> either(json_string_parser(options), ecmascript_identifier_parser())
    False -> json_string_parser(options)
  }
  allow_comments(key_parser)
  |> left(allow_comments(symbol(":", options)))
  |> pair(json_value_parser)
  |> sep(allow_comments(symbol(",", options)))
  |> map(dict.from_list)
  |> between(
    allow_comments(symbol("{", options)),
    allow_trailing_comma(options, symbol("}", options)),
  )
}

fn json_value_parser(options: JsonParseOptions) -> JsonParser(JsonValue) {
  choice([
    to(json_null_parser(), Null),
    map(json_boolean_parser(), Boolean),
    map(json_number_parser(options), Number),
    map(json_string_parser(options), String),
    map(json_array_parser(options), Array),
    map(json_object_parser(options), Object),
  ])
  |> padded(options)
  |> allow_comments(options, _)
}

fn json_parser(options: JsonParseOptions) -> JsonParser(JsonValue) {
  json_value_parser(options)
  |> between(allow_comments(options, ws(options)), end())
}

pub type JsonParseOptions {
  JsonParseOptions(
    comments: Bool,
    trailing_comma: Bool,
    ecma_object_keys: Bool,
    ecma_strings: Bool,
    ecma_numbers: Bool,
    ecma_whitespace: Bool,
  )
}

const json_options = JsonParseOptions(
  comments: False,
  trailing_comma: False,
  ecma_object_keys: False,
  ecma_strings: False,
  ecma_numbers: False,
  ecma_whitespace: False,
)

const jsonc_options = JsonParseOptions(
  comments: True,
  trailing_comma: False,
  ecma_object_keys: False,
  ecma_strings: False,
  ecma_numbers: False,
  ecma_whitespace: False,
)

const json5_options = JsonParseOptions(
  comments: True,
  trailing_comma: True,
  ecma_object_keys: True,
  ecma_strings: True,
  ecma_numbers: True,
  ecma_whitespace: True,
)

const jsonl_options = json_options

pub fn parse_json_custom(
  value: String,
  options: JsonParseOptions,
) -> Result(JsonValue, JsonParserError) {
  go(json_parser(options), value)
}

pub fn parse_json(value: String) -> Result(JsonValue, JsonParserError) {
  parse_json_custom(value, json_options)
}

pub fn parse_jsonc(value: String) -> Result(JsonValue, JsonParserError) {
  parse_json_custom(value, jsonc_options)
}

pub fn parse_json5(value: String) -> Result(JsonValue, JsonParserError) {
  parse_json_custom(value, json5_options)
}

fn split_jsonl(value: String) -> List(String) {
  case string.last(value) {
    Ok("\n") -> string.drop_right(value, 1)
    _ -> value
  }
  |> string.split("\n")
}

pub fn parse_jsonl(value: String) -> Result(List(JsonValue), JsonParserError) {
  let parse = go(json_parser(jsonl_options), _)
  list.try_map(split_jsonl(value), parse)
}

pub fn parse_jsonl_all(
  value: String,
) -> List(Result(JsonValue, JsonParserError)) {
  let parse = go(json_parser(jsonl_options), _)
  list.map(split_jsonl(value), parse)
}

pub fn parse_jsonl_valid(value: String) -> List(JsonValue) {
  value
  |> parse_jsonl_all
  |> result.values
}

fn stringify_json_spaced_rec(
  value: JsonValue,
  space: String,
  depth: Int,
) -> String {
  case value {
    Object(obj) ->
      case dict.to_list(obj) {
        [] -> "{}"
        ls ->
          "{\n"
          <> string.repeat(space, depth + 1)
          <> {
            list.map(ls, fn(kv) {
              "\""
              <> kv.0
              <> "\": "
              <> stringify_json_spaced_rec(kv.1, space, depth + 1)
            })
            |> string.join(",\n" <> string.repeat(space, depth + 1))
          }
          <> "\n"
          <> string.repeat(space, depth)
          <> "}"
      }
    Array([]) -> "[]"
    Array(arr) ->
      "[\n"
      <> string.repeat(space, depth + 1)
      <> {
        arr
        |> list.map(stringify_json_spaced_rec(_, space, depth + 1))
        |> string.join(",\n" <> string.repeat(space, depth + 1))
      }
      <> "\n"
      <> string.repeat(space, depth)
      <> "]"
    String(str) -> "\"" <> str <> "\""
    Number(flt) -> float.to_string(flt)
    Boolean(True) -> "true"
    Boolean(False) -> "false"
    Null -> "null"
  }
}

pub type Indentation {
  Spaces(Int)
  Tab
  Tabs(Int)
}

pub fn stringify_json_spaced(
  value: JsonValue,
  indentation: Indentation,
) -> String {
  stringify_json_spaced_rec(
    value,
    case indentation {
      Spaces(n) -> string.repeat(" ", n)
      Tab -> "\t"
      Tabs(n) -> string.repeat("\t", n)
    },
    0,
  )
}

pub fn stringify_json(value: JsonValue) -> String {
  case value {
    Object(obj) ->
      "{"
      <> {
        obj
        |> dict.to_list
        |> list.map(fn(kv) { "\"" <> kv.0 <> "\":" <> stringify_json(kv.1) })
        |> string.join(",")
      }
      <> "}"
    Array(arr) ->
      "["
      <> {
        arr
        |> list.map(stringify_json)
        |> string.join(",")
      }
      <> "]"
    String(str) -> "\"" <> str <> "\""
    Number(flt) -> float.to_string(flt)
    Boolean(True) -> "true"
    Boolean(False) -> "false"
    Null -> "null"
  }
}

pub fn stringify_jsonl(values: List(JsonValue)) -> String {
  values
  |> list.map(stringify_json)
  |> string.join("\n")
}

pub type JsonQuery {
  Root
  Key(query: JsonQuery, key: String)
  KeyOr(query: JsonQuery, key: String, or: JsonValue)
  Index(query: JsonQuery, index: Int)
  IndexOr(query: JsonQuery, index: Int, or: JsonValue)
  Filter(query: JsonQuery, predicate: fn(JsonValue) -> Bool)
  Map(query: JsonQuery, mapping: fn(JsonValue) -> JsonValue)
  MapKeys(query: JsonQuery, mapping: fn(String) -> String)
  MapValues(query: JsonQuery, mapping: fn(String, JsonValue) -> JsonValue)
  FilterMap(query: JsonQuery, mapping: fn(JsonValue) -> Result(JsonValue, Nil))
  ForEach(query: JsonQuery)
  ForEachOk(query: JsonQuery)
}

type InvJsonQuery {
  InvEnd
  InvKey(key: String, query: InvJsonQuery)
  InvKeyOr(key: String, or: JsonValue, query: InvJsonQuery)
  InvIndex(index: Int, query: InvJsonQuery)
  InvIndexOr(index: Int, or: JsonValue, query: InvJsonQuery)
  InvFilter(predicate: fn(JsonValue) -> Bool, query: InvJsonQuery)
  InvMap(mapping: fn(JsonValue) -> JsonValue, query: InvJsonQuery)
  InvMapKeys(mapping: fn(String) -> String, query: InvJsonQuery)
  InvMapValues(mapping: fn(String, JsonValue) -> JsonValue, query: InvJsonQuery)
  InvFilterMap(
    mapping: fn(JsonValue) -> Result(JsonValue, Nil),
    query: InvJsonQuery,
  )
  InvForEach(query: InvJsonQuery)
  InvForEachOk(query: InvJsonQuery)
}

fn invert_query_rec(query: JsonQuery, state: InvJsonQuery) -> InvJsonQuery {
  case query {
    Root -> state
    Key(query, key) -> invert_query_rec(query, InvKey(key, state))
    KeyOr(query, key, o) -> invert_query_rec(query, InvKeyOr(key, o, state))
    Index(query, index) -> invert_query_rec(query, InvIndex(index, state))
    IndexOr(query, index, or) ->
      invert_query_rec(query, InvIndexOr(index, or, state))
    Filter(query, predicate) ->
      invert_query_rec(query, InvFilter(predicate, state))
    Map(query, mapping) -> invert_query_rec(query, InvMap(mapping, state))
    MapKeys(query, mapping) ->
      invert_query_rec(query, InvMapKeys(mapping, state))
    MapValues(query, mapping) ->
      invert_query_rec(query, InvMapValues(mapping, state))
    FilterMap(query, mapping) ->
      invert_query_rec(query, InvFilterMap(mapping, state))
    ForEach(query) -> invert_query_rec(query, InvForEach(state))
    ForEachOk(query) -> invert_query_rec(query, InvForEachOk(state))
  }
}

fn invert_query(query: JsonQuery) -> InvJsonQuery {
  invert_query_rec(query, InvEnd)
}

// list.at has been removed from gleam/list
// https://github.com/gleam-lang/stdlib/commit/b98705f890a2828c48dfdab291a22f145309d646
fn list_at(in list: List(a), get index: Int) -> Result(a, Nil) {
  case index >= 0 {
    True -> list |> list.drop(index) |> list.first
    False -> Error(Nil)
  }
}

pub type JsonQueryError {
  UnexpectedType(JsonValue)
  MissingObjectKey(JsonValue, key: String)
  IndexOutOfBounds(JsonValue, index: Int)
}

fn query_json_rec(
  json: JsonValue,
  query: InvJsonQuery,
) -> Result(JsonValue, JsonQueryError) {
  case query {
    InvEnd -> Ok(json)
    InvKey(key, q) ->
      case json {
        Object(obj) as j ->
          obj
          |> dict.get(key)
          |> result.replace_error(MissingObjectKey(j, key))
        j -> Error(UnexpectedType(j))
      }
      |> result.map(query_json_rec(_, q))
      |> result.flatten
    InvKeyOr(key, or, q) ->
      case json {
        Object(obj) ->
          obj
          |> dict.get(key)
          |> result.unwrap(or)
          |> Ok
        j -> Error(UnexpectedType(j))
      }
      |> result.map(query_json_rec(_, q))
      |> result.flatten
    InvIndex(index, q) ->
      case json {
        Array(arr) as j ->
          arr
          |> list_at(index)
          |> result.replace_error(IndexOutOfBounds(j, index))
        j -> Error(UnexpectedType(j))
      }
      |> result.map(query_json_rec(_, q))
      |> result.flatten
    InvIndexOr(index, or, q) ->
      case json {
        Array(arr) ->
          arr
          |> list_at(index)
          |> result.unwrap(or)
          |> Ok
        j -> Error(UnexpectedType(j))
      }
      |> result.map(query_json_rec(_, q))
      |> result.flatten
    InvFilter(predicate, q) ->
      case json {
        Array(arr) ->
          arr
          |> list.filter(predicate)
          |> Array
          |> query_json_rec(q)
        j -> Error(UnexpectedType(j))
      }
    InvMap(mapping, q) ->
      case json {
        Array(arr) ->
          arr
          |> list.map(mapping)
          |> Array
          |> query_json_rec(q)
        j -> Error(UnexpectedType(j))
      }
    InvMapKeys(mapping, q) ->
      case json {
        Object(obj) ->
          obj
          |> dict.to_list
          |> list.map(fn(kv) { #(mapping(kv.0), kv.1) })
          |> dict.from_list
          |> Object
          |> query_json_rec(q)
        j -> Error(UnexpectedType(j))
      }
    InvMapValues(mapping, q) ->
      case json {
        Object(obj) ->
          obj
          |> dict.map_values(mapping)
          |> Object
          |> query_json_rec(q)
        j -> Error(UnexpectedType(j))
      }
    InvFilterMap(mapping, q) ->
      case json {
        Array(arr) ->
          arr
          |> list.filter_map(mapping)
          |> Array
          |> query_json_rec(q)
        j -> Error(UnexpectedType(j))
      }
    InvForEach(q) ->
      case json {
        Array(arr) ->
          arr
          |> list.map(query_json_rec(_, q))
          |> result.all
          |> result.map(Array)
        j -> Error(UnexpectedType(j))
      }
    InvForEachOk(q) ->
      case json {
        Array(arr) ->
          arr
          |> list.map(query_json_rec(_, q))
          |> result.values
          |> Array
          |> Ok
        j -> Error(UnexpectedType(j))
      }
  }
}

pub fn query_json(json: JsonValue, query: JsonQuery) {
  query_json_rec(json, invert_query(query))
}
