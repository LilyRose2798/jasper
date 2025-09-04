import gleam/dict
import gleeunit
import jasper.{
  Array, Bool, Float, Index, IndexOutOfBounds, Int, Key, MissingObjectKey, Null,
  Object, Root, String, UnexpectedType,
}

pub fn main() {
  gleeunit.main()
}

pub fn parse_float_test() {
  assert jasper.parse("4.2") == Ok(Float(4.2))
}

pub fn parse_int_test() {
  assert jasper.parse("42") == Ok(Int(42))
}

pub fn parse_bool_true_test() {
  assert jasper.parse("true") == Ok(Bool(True))
}

pub fn parse_bool_false_test() {
  assert jasper.parse("false") == Ok(Bool(False))
}

pub fn parse_null_test() {
  assert jasper.parse("null") == Ok(Null)
}

pub fn parse_strings_test() {
  assert jasper.parse("\"hello\"") == Ok(String("hello"))
}

pub fn parse_arrays_test() {
  assert jasper.parse("[]") == Ok(Array([]))
  assert jasper.parse("[1, 2, 3]") == Ok(Array([Int(1), Int(2), Int(3)]))
  assert jasper.parse("[true, false, null]")
    == Ok(Array([Bool(True), Bool(False), Null]))
  assert jasper.parse("[\"hello\", \"world\"]")
    == Ok(Array([String("hello"), String("world")]))
}

pub fn parse_objects_test() {
  assert jasper.parse("{}") == Ok(Object(dict.new()))
  assert jasper.parse("{\"a\": 1, \"b\": 2}")
    == Ok(Object(dict.from_list([#("a", Int(1)), #("b", Int(2))])))
  assert jasper.parse("{\"a\": true, \"b\": false, \"c\": null}")
    == Ok(
      Object(
        dict.from_list([#("a", Bool(True)), #("b", Bool(False)), #("c", Null)]),
      ),
    )
  assert jasper.parse("{\"a\": \"hello\", \"b\": \"world\"}")
    == Ok(
      Object(dict.from_list([#("a", String("hello")), #("b", String("world"))])),
    )
  assert jasper.parse("{\"👋\": [1, 2, 3], \"b\": {\"c\": 4}}")
    == Ok(
      Object(
        dict.from_list([
          #("👋", Array([Int(1), Int(2), Int(3)])),
          #("b", Object(dict.from_list([#("c", Int(4))]))),
        ]),
      ),
    )
}

pub fn query_test() {
  assert jasper.query(String("foo"), Root) == Ok(String("foo"))
  assert jasper.query(
      String("foo"),
      Root
        |> Key("foo"),
    )
    == Error(UnexpectedType(String("foo")))
  assert jasper.query(
      String("foo"),
      Root
        |> Index(2),
    )
    == Error(UnexpectedType(String("foo")))
  assert jasper.query(
      Array([String("foo")]),
      Root
        |> Index(2),
    )
    == Error(IndexOutOfBounds(Array([String("foo")]), 2))
  assert jasper.query(
      Object(dict.from_list([#("bar", Array([String("foo")]))])),
      Root
        |> Key("bar")
        |> Index(2),
    )
    == Error(IndexOutOfBounds(Array([String("foo")]), 2))
  assert jasper.query(
      Object(dict.from_list([#("bar", Array([String("foo")]))])),
      Root
        |> Key("foo")
        |> Index(2),
    )
    == Error(MissingObjectKey(
      Object(dict.from_list([#("bar", Array([String("foo")]))])),
      "foo",
    ))
}
