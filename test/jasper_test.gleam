import gleeunit
import gleeunit/should
import gleam/dict
import jasper.{
  type JsonValue, Array, Boolean, Index, IndexOutOfBounds, Key, MissingObjectKey,
  Null, Number, Object, Root, String, UnexpectedType, parse_json, query_json,
}

pub fn main() {
  gleeunit.main()
}

fn should_parse(json: String, result: JsonValue) {
  json
  |> parse_json
  |> should.equal(Ok(result))
}

pub fn parse_numbers_test() {
  should_parse("4.2", Number(4.2))
  should_parse("42", Number(42.0))
}

pub fn parse_booleans_test() {
  should_parse("true", Boolean(True))
  should_parse("false", Boolean(False))
}

pub fn parse_null_test() {
  should_parse("null", Null)
}

pub fn parse_strings_test() {
  should_parse("\"hello\"", String("hello"))
}

pub fn parse_arrays_test() {
  should_parse("[]", Array([]))
  should_parse("[1, 2, 3]", Array([Number(1.0), Number(2.0), Number(3.0)]))
  should_parse(
    "[true, false, null]",
    Array([Boolean(True), Boolean(False), Null]),
  )
  should_parse(
    "[\"hello\", \"world\"]",
    Array([String("hello"), String("world")]),
  )
}

pub fn parse_objects_test() {
  should_parse("{}", Object(dict.new()))
  should_parse(
    "{\"a\": 1, \"b\": 2}",
    Object(dict.from_list([#("a", Number(1.0)), #("b", Number(2.0))])),
  )
  should_parse(
    "{\"a\": true, \"b\": false, \"c\": null}",
    Object(
      dict.from_list([
        #("a", Boolean(True)),
        #("b", Boolean(False)),
        #("c", Null),
      ]),
    ),
  )
  should_parse(
    "{\"a\": \"hello\", \"b\": \"world\"}",
    Object(dict.from_list([#("a", String("hello")), #("b", String("world"))])),
  )
  should_parse(
    "{\"👋\": [1, 2, 3], \"b\": {\"c\": 4}}",
    Object(
      dict.from_list([
        #("👋", Array([Number(1.0), Number(2.0), Number(3.0)])),
        #("b", Object(dict.from_list([#("c", Number(4.0))]))),
      ]),
    ),
  )
}

pub fn query_test() {
  query_json(String("foo"), Root)
  |> should.equal(Ok(String("foo")))
  query_json(
    String("foo"),
    Root
      |> Key("foo"),
  )
  |> should.equal(Error(UnexpectedType(String("foo"))))
  query_json(
    String("foo"),
    Root
      |> Index(2),
  )
  |> should.equal(Error(UnexpectedType(String("foo"))))
  query_json(
    Array([String("foo")]),
    Root
      |> Index(2),
  )
  |> should.equal(Error(IndexOutOfBounds(Array([String("foo")]), 2)))
  query_json(
    Object(dict.from_list([#("bar", Array([String("foo")]))])),
    Root
      |> Key("bar")
      |> Index(2),
  )
  |> should.equal(Error(IndexOutOfBounds(Array([String("foo")]), 2)))
  query_json(
    Object(dict.from_list([#("bar", Array([String("foo")]))])),
    Root
      |> Key("foo")
      |> Index(2),
  )
  |> should.equal(
    Error(MissingObjectKey(
      Object(dict.from_list([#("bar", Array([String("foo")]))])),
      "foo",
    )),
  )
}
