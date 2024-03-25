import gleam/float
import gleam/int
import gleam/string
import gleam/result
import gleam/option.{None, Some}
import gleam/dict.{type Dict}
import gleam/list
import pears.{type Parser}
import pears/chars.{type Char, digit, string}
import pears/combinators.{
  alt, between, choice, eof, just, lazy, left, many0, many1, map, maybe, none_of,
  one_of, pair, recognize, right, sep_by0, seq, to,
}

pub type ParseError {
  UnexpectedToken(found: Char)
  UnexpectedEndOfInput
}

fn run_parser(
  input: String,
  parser: pears.Parser(Char, a),
) -> Result(a, ParseError) {
  case parser(chars.input(input)) {
    Ok(pears.Parsed(_, j)) -> Ok(j)
    Error(e) ->
      Error(case e {
        pears.UnexpectedToken(_, _, f) -> UnexpectedToken(f)
        pears.UnexpectedEndOfInput(_, _) -> UnexpectedEndOfInput
      })
  }
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

fn ws0() -> Parser(Char, List(Char)) {
  one_of([" ", "\n", "\r", "\t"])
  |> many0()
}

fn padded(p: Parser(_, a)) {
  left(p, ws0())
}

fn symbol(s: String) {
  padded(string(s))
}

fn value_parser() -> Parser(Char, JsonValue) {
  let hex_digit =
    one_of([
      "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e",
      "f", "A", "B", "C", "D", "E", "F",
    ])

  let unicode_escape_digits =
    recognize(seq([hex_digit, hex_digit, hex_digit, hex_digit]))

  let escape =
    just("\\")
    |> right(
      choice([
        just("\\"),
        just("/"),
        just("\""),
        to(just("b"), "\u{0008}"),
        to(just("f"), "\u{000C}"),
        to(just("n"), "\n"),
        to(just("r"), "\r"),
        to(just("t"), "\t"),
        map(right(just("u"), unicode_escape_digits), fn(value) {
          let assert Ok(number) = int.base_parse(string.concat(value), 16)
          let assert Ok(codepoint) = string.utf_codepoint(number)
          string.from_utf_codepoints([codepoint])
        }),
      ]),
    )

  let str =
    none_of(["\"", "\\"])
    |> alt(escape)
    |> many0()
    |> map(string.concat)
    |> between(just("\""), just("\""))

  let value = lazy(value_parser)

  let num =
    maybe(just("-"))
    |> pair(
      alt(
        to(just("0"), ["0"]),
        recognize(pair(
          one_of(["1", "2", "3", "4", "5", "6", "7", "8", "9"]),
          many0(digit()),
        )),
      )
      |> map(string.concat),
    )
    |> pair(maybe(
      just(".")
      |> right(many1(digit()))
      |> map(string.concat),
    ))
    |> pair(
      recognize(maybe(
        alt(just("e"), just("E"))
        |> pair(maybe(one_of(["+", "-"])))
        |> pair(many1(digit())),
      ))
      |> map(string.concat),
    )
    |> map(fn(p) {
      case p {
        #(#(#(neg, ns), ds), ex) -> {
          {
            option.unwrap(neg, "") <> ns <> "." <> option.unwrap(ds, "0") <> ex
          }
          |> float.parse
          |> result.unwrap(case neg {
            Some(_) -> -1.7976931348623158e308
            None -> 1.7976931348623158e308
          })
          |> Number
        }
      }
    })

  let bool =
    alt(to(string("true"), Boolean(True)), to(string("false"), Boolean(False)))

  let null = to(string("null"), Null)

  let array =
    sep_by0(value, symbol(","))
    |> between(symbol("["), symbol("]"))
    |> map(Array)

  let obj =
    str
    |> left(symbol(":"))
    |> pair(value)
    |> sep_by0(symbol(","))
    |> map(dict.from_list)
    |> between(symbol("{"), symbol("}"))
    |> map(Object)

  choice([num, bool, null, map(str, String), array, obj])
  |> padded()
}

fn json_parser() -> Parser(Char, JsonValue) {
  value_parser()
  |> between(ws0(), eof())
}

pub fn parse_json(value: String) -> Result(JsonValue, ParseError) {
  run_parser(value, json_parser())
}

fn split_jsonl(value: String) -> List(String) {
  case string.last(value) {
    Ok("\n") -> string.drop_right(value, 1)
    _ -> value
  }
  |> string.split("\n")
}

pub fn parse_jsonl(value: String) -> Result(List(JsonValue), ParseError) {
  let parse = run_parser(_, json_parser())
  list.try_map(split_jsonl(value), parse)
}

pub fn parse_jsonl_all(value: String) -> List(Result(JsonValue, ParseError)) {
  let parse = run_parser(_, json_parser())
  list.map(split_jsonl(value), parse)
}

pub fn parse_jsonl_valid(value: String) -> List(JsonValue) {
  value
  |> parse_jsonl_all
  |> result.values
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
