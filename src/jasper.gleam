import gleam/float
import gleam/int
import gleam/string
import gleam/result
import gleam/dict.{type Dict}
import gleam/list
import party.{
  type ParseError, type Parser, char, choice, digit, do, either, end, go, lazy,
  letter, many1_concat, many_concat, map, perhaps, return, satisfy, sep, string,
  until,
}

pub fn to(p: Parser(a, e), v: b) -> Parser(b, e) {
  use _ <- do(p)
  return(v)
}

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

fn multiline_comment() -> Parser(String, e) {
  use _ <- do(string("/*"))
  use comment <- do(until(satisfy(fn(_) { True }), string("*/")))
  return(string.concat(comment))
}

fn singleline_comment() -> Parser(String, e) {
  use _ <- do(string("//"))
  use comment <- do(
    many_concat(satisfy(fn(c) { !string.contains("\r\n\u{2028}\u{2029}", c) })),
  )
  return(comment)
}

fn comment() -> Parser(String, e) {
  either(multiline_comment(), singleline_comment())
}

fn ws(options: JsonParseOptions) -> Parser(String, e) {
  let ws =
    satisfy(fn(c) {
      string.contains(
        case options.ecma_whitespace {
          True ->
            " \n\r\t\u{000B}\u{000C}\u{00A0}\u{2028}\u{2029}\u{FEFF}\u{1680}\u{2000}\u{2001}\u{2002}\u{2003}\u{2004}\u{2005}\u{2006}\u{2007}\u{2008}\u{2009}\u{200A}\u{202F}\u{205F}\u{3000}"
          False -> " \n\r\t"
        },
        c,
      )
    })
  many_concat(case options.comments {
    True -> either(ws, comment())
    False -> ws
  })
}

fn symbol(s: String, options: JsonParseOptions) -> Parser(String, e) {
  use _ <- do(ws(options))
  use c <- do(char(s))
  return(c)
}

fn json_null() -> Parser(Nil, e) {
  to(string("null"), Nil)
}

fn json_boolean() -> Parser(Bool, e) {
  either(to(string("true"), True), to(string("false"), False))
}

fn hex_digit() -> Parser(String, e) {
  satisfy(fn(c) { string.contains("0123456789abcdefABCDEF", c) })
}

fn json_number(options: JsonParseOptions) -> Parser(Float, e) {
  let sign_parser = perhaps(either(char("+"), char("-")))
  let int_parser =
    either(char("0"), {
      use d <- do(satisfy(fn(c) { string.contains("123456789", c) }))
      use ds <- do(many_concat(digit()))
      return(d <> ds)
    })
  let exp_parser =
    perhaps({
      use e <- do(either(char("e"), char("E")))
      use sign <- do(sign_parser)
      use digits <- do(many1_concat(digit()))
      return(e <> result.unwrap(sign, "") <> digits)
    })
  let min_double = -1.7976931348623158e308
  let max_double = 1.7976931348623158e308

  case options.ecma_numbers {
    True -> {
      use sign <- do(sign_parser)
      let inf = case sign {
        Ok("-") -> min_double
        _ -> max_double
      }
      choice([
        to(string("Infinity"), inf),
        to(string("NaN"), 0.0),
        {
          use _ <- do(either(string("0x"), string("0X")))
          use hex <- do(many1_concat(hex_digit()))
          let assert Ok(n) = int.base_parse(result.unwrap(sign, "") <> hex, 16)
          return(int.to_float(n))
        },
        {
          use n <- do(
            either(
              {
                use ns <- do(int_parser)
                use ds <- do(
                  perhaps({
                    use _ <- do(char("."))
                    use ds <- do(perhaps(many1_concat(digit())))
                    return(result.unwrap(ds, "0"))
                  }),
                )
                return(ns <> "." <> result.unwrap(ds, "0"))
              },
              {
                use _ <- do(char("."))
                use ds <- do(many1_concat(digit()))
                return("0." <> ds)
              },
            ),
          )
          use ex <- do(exp_parser)
          { result.unwrap(sign, "") <> n <> result.unwrap(ex, "") }
          |> float.parse
          |> result.unwrap(inf)
          |> return
        },
      ])
    }
    False -> {
      use sign <- do(perhaps(char("-")))
      let inf = case sign {
        Ok("-") -> min_double
        _ -> max_double
      }
      use ns <- do(int_parser)
      use ds <- do(
        perhaps({
          use _ <- do(char("."))
          use ds <- do(many1_concat(digit()))
          return(ds)
        }),
      )
      use ex <- do(exp_parser)
      {
        result.unwrap(sign, "")
        <> ns
        <> "."
        <> result.unwrap(ds, "0")
        <> result.unwrap(ex, "")
      }
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

fn json_string(options: JsonParseOptions) -> Parser(String, e) {
  let unicode_escape = {
    use _ <- do(char("u"))
    use a <- do(hex_digit())
    use b <- do(hex_digit())
    use c <- do(hex_digit())
    use d <- do(hex_digit())
    return(string_to_codepoint(a <> b <> c <> d))
  }

  let escape = {
    use _ <- do(char("\\"))
    use c <- do(
      choice(case options.ecma_strings {
        True -> [
          {
            use _ <- do(char("x"))
            use a <- do(hex_digit())
            use b <- do(hex_digit())
            return(string_to_codepoint(a <> b))
          },
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
    return(c)
  }

  let str = fn(q) {
    use _ <- do(char(q))
    use str <- do(
      many_concat(either(
        escape,
        satisfy(fn(c) { c != q && c != "\\" && valid_string_char(c) }),
      )),
    )
    use _ <- do(char(q))
    return(str)
  }

  case options.ecma_strings {
    True -> either(str("\""), str("'"))
    False -> str("\"")
  }
}

fn allow_trailing_comma(p: Parser(a, e), options: JsonParseOptions) {
  case options.trailing_comma {
    True -> {
      use _ <- do(perhaps(symbol(",", options)))
      use v <- do(p)
      return(v)
    }
    False -> p
  }
}

fn json_array(options: JsonParseOptions) -> Parser(JsonArray, e) {
  use _ <- do(symbol("[", options))
  use values <- do(sep(lazy(fn() { json_value(options) }), symbol(",", options)))
  use _ <- do(allow_trailing_comma(symbol("]", options), options))
  return(values)
}

// todo
fn ecmascript_identifier() -> Parser(String, e) {
  use c <- do(letter())
  use cs <- do(many_concat(either(letter(), digit())))
  return(c <> cs)
}

fn json_object(options: JsonParseOptions) -> Parser(JsonObject, e) {
  use _ <- do(symbol("{", options))
  use key_value_pairs <- do(sep(
    {
      use _ <- do(ws(options))
      use key <- do(case options.ecma_object_keys {
        True -> either(json_string(options), ecmascript_identifier())
        False -> json_string(options)
      })
      use _ <- do(symbol(":", options))
      use value <- do(lazy(fn() { json_value(options) }))
      return(#(key, value))
    },
    symbol(",", options),
  ))
  use _ <- do(allow_trailing_comma(symbol("}", options), options))
  return(dict.from_list(key_value_pairs))
}

fn json_value(options: JsonParseOptions) -> Parser(JsonValue, e) {
  use _ <- do(ws(options))
  choice([
    to(json_null(), Null),
    map(json_boolean(), Boolean),
    map(json_number(options), Number),
    map(json_string(options), String),
    map(json_array(options), Array),
    map(json_object(options), Object),
  ])
}

fn json_parser(options: JsonParseOptions) -> Parser(JsonValue, e) {
  use value <- do(json_value(options))
  use _ <- do(ws(options))
  use _ <- do(end())
  return(value)
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
) -> Result(JsonValue, ParseError(e)) {
  go(json_parser(options), value)
}

pub fn parse_json(value: String) -> Result(JsonValue, ParseError(e)) {
  parse_json_custom(value, json_options)
}

pub fn parse_jsonc(value: String) -> Result(JsonValue, ParseError(e)) {
  parse_json_custom(value, jsonc_options)
}

pub fn parse_json5(value: String) -> Result(JsonValue, ParseError(e)) {
  parse_json_custom(value, json5_options)
}

fn split_jsonl(value: String) -> List(String) {
  case string.last(value) {
    Ok("\n") -> string.drop_right(value, 1)
    _ -> value
  }
  |> string.split("\n")
}

pub fn parse_jsonl(value: String) -> Result(List(JsonValue), ParseError(e)) {
  let parse = go(json_parser(jsonl_options), _)
  list.try_map(split_jsonl(value), parse)
}

pub fn parse_jsonl_all(value: String) -> List(Result(JsonValue, ParseError(e))) {
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
          |> list.at(index)
          |> result.replace_error(IndexOutOfBounds(j, index))
        j -> Error(UnexpectedType(j))
      }
      |> result.map(query_json_rec(_, q))
      |> result.flatten
    InvIndexOr(index, or, q) ->
      case json {
        Array(arr) ->
          arr
          |> list.at(index)
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
