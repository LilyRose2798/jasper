import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/float
import gleam/function
import gleam/int
import gleam/json.{type DecodeError, type Json}
import gleam/list
import gleam/result
import gleam/string
import gleam/string_tree.{type StringTree}

pub type JsonValue {
  Object(Dict(String, JsonValue))
  Array(List(JsonValue))
  String(String)
  Int(Int)
  Float(Float)
  Bool(Bool)
  Null
}

pub fn decoder() -> Decoder(JsonValue) {
  decode.one_of(decode.string |> decode.map(String), [
    decode.int |> decode.map(Int),
    decode.float |> decode.map(Float),
    decode.bool |> decode.map(Bool),
    decode.list(decode.recursive(decoder)) |> decode.map(Array),
    decode.dict(decode.string, decode.recursive(decoder))
      |> decode.map(Object),
    decode.success(Null),
  ])
}

pub fn parse(value: String) -> Result(JsonValue, DecodeError) {
  json.parse(value, decoder())
}

pub fn parse_bits(value: BitArray) -> Result(JsonValue, DecodeError) {
  json.parse_bits(value, decoder())
}

pub fn to_json(value: JsonValue) -> Json {
  case value {
    String(s) -> json.string(s)
    Int(i) -> json.int(i)
    Float(f) -> json.float(f)
    Bool(b) -> json.bool(b)
    Array(a) -> json.array(a, to_json)
    Object(o) -> json.dict(o, function.identity, to_json)
    Null -> json.null()
  }
}

pub fn to_string(value: JsonValue) -> String {
  to_json(value) |> json.to_string
}

pub fn to_string_tree(value: JsonValue) -> StringTree {
  to_json(value) |> json.to_string_tree
}

pub type Indentation {
  Spaces(Int)
  Tab
  Tabs(Int)
}

pub fn to_pretty_string(value: JsonValue, indentation: Indentation) -> String {
  do_to_pretty_string(
    value,
    case indentation {
      Spaces(n) -> string.repeat(" ", n)
      Tab -> "\t"
      Tabs(n) -> string.repeat("\t", n)
    },
    0,
  )
}

fn do_to_pretty_string(value: JsonValue, space: String, depth: Int) -> String {
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
              <> do_to_pretty_string(kv.1, space, depth + 1)
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
        |> list.map(do_to_pretty_string(_, space, depth + 1))
        |> string.join(",\n" <> string.repeat(space, depth + 1))
      }
      <> "\n"
      <> string.repeat(space, depth)
      <> "]"
    String(s) -> "\"" <> s <> "\""
    Int(i) -> int.to_string(i)
    Float(f) -> float.to_string(f)
    Bool(True) -> "true"
    Bool(False) -> "false"
    Null -> "null"
  }
}

pub fn to_pretty_string_tree(
  value: JsonValue,
  indentation: Indentation,
) -> StringTree {
  to_pretty_string(value, indentation) |> string_tree.from_string
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

fn do_query(
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
      |> result.map(do_query(_, q))
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
      |> result.map(do_query(_, q))
      |> result.flatten
    InvIndex(index, q) ->
      case json {
        Array(arr) as j ->
          arr
          |> list.drop(index)
          |> list.first()
          |> result.replace_error(IndexOutOfBounds(j, index))
        j -> Error(UnexpectedType(j))
      }
      |> result.map(do_query(_, q))
      |> result.flatten
    InvIndexOr(index, or, q) ->
      case json {
        Array(arr) ->
          arr
          |> list.drop(index)
          |> list.first()
          |> result.unwrap(or)
          |> Ok
        j -> Error(UnexpectedType(j))
      }
      |> result.map(do_query(_, q))
      |> result.flatten
    InvFilter(predicate, q) ->
      case json {
        Array(arr) ->
          arr
          |> list.filter(predicate)
          |> Array
          |> do_query(q)
        j -> Error(UnexpectedType(j))
      }
    InvMap(mapping, q) ->
      case json {
        Array(arr) ->
          arr
          |> list.map(mapping)
          |> Array
          |> do_query(q)
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
          |> do_query(q)
        j -> Error(UnexpectedType(j))
      }
    InvMapValues(mapping, q) ->
      case json {
        Object(obj) ->
          obj
          |> dict.map_values(mapping)
          |> Object
          |> do_query(q)
        j -> Error(UnexpectedType(j))
      }
    InvFilterMap(mapping, q) ->
      case json {
        Array(arr) ->
          arr
          |> list.filter_map(mapping)
          |> Array
          |> do_query(q)
        j -> Error(UnexpectedType(j))
      }
    InvForEach(q) ->
      case json {
        Array(arr) ->
          arr
          |> list.map(do_query(_, q))
          |> result.all
          |> result.map(Array)
        j -> Error(UnexpectedType(j))
      }
    InvForEachOk(q) ->
      case json {
        Array(arr) ->
          arr
          |> list.map(do_query(_, q))
          |> result.values
          |> Array
          |> Ok
        j -> Error(UnexpectedType(j))
      }
  }
}

pub fn query(json: JsonValue, query: JsonQuery) {
  do_query(json, invert_query(query))
}
