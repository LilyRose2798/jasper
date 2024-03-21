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

fn whitespace0() -> Parser(Char, List(Char)) {
  one_of([" ", "\n", "\r", "\t"])
  |> many0()
}

fn value_parser() -> Parser(Char, JsonValue) {
  let padded = fn(parser: Parser(_, a)) { left(parser, whitespace0()) }
  let symbol = fn(s: String) { padded(string(s)) }

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
    none_of(["\""])
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
  |> between(whitespace0(), eof())
}

pub type JsonParseError {
  UnexpectedToken(found: Char)
  UnexpectedEndOfInput
}

pub fn parse_json(value: String) -> Result(JsonValue, JsonParseError) {
  case json_parser()(chars.input(value)) {
    Ok(pears.Parsed(_, j)) -> Ok(j)
    Error(e) ->
      Error(case e {
        pears.UnexpectedToken(_, _, f) -> UnexpectedToken(f)
        pears.UnexpectedEndOfInput(_, _) -> UnexpectedEndOfInput
      })
  }
}

pub type JsonQuery {
  Root
  Key(JsonQuery, key: String)
  Index(JsonQuery, index: Int)
}

pub type JsonQueryError {
  UnexpectedType(JsonValue)
  MissingObjectKey(JsonValue, key: String)
  IndexOutOfBounds(JsonValue, index: Int)
}

pub fn query_json(
  json: JsonValue,
  query: JsonQuery,
) -> Result(JsonValue, JsonQueryError) {
  case query {
    Root -> Ok(json)
    Key(q, k) ->
      case query_json(json, q) {
        Ok(Object(o) as j) ->
          dict.get(o, k)
          |> result.replace_error(MissingObjectKey(j, k))
        Ok(j) -> Error(UnexpectedType(j))
        x -> x
      }
    Index(q, i) ->
      case query_json(json, q) {
        Ok(Array(a) as j) ->
          list.at(a, i)
          |> result.replace_error(IndexOutOfBounds(j, i))
        Ok(j) -> Error(UnexpectedType(j))
        x -> x
      }
  }
}
