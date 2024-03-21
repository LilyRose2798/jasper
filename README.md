# jasper

[![Package Version](https://img.shields.io/hexpm/v/jasper)](https://hex.pm/packages/jasper)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/jasper/)

```sh
gleam add jasper
```
```gleam
import gleam/io
import jasper.{parse_json, query_json, String, Root, Key, Index}

pub fn main() {
  let assert Ok(json) = parse_json("{ \"foo\": [1, true, \"hi\"] }")
  let assert Ok(String(str)) = query_json(json, Root |> Key("foo") |> Index(2))
  io.println(str)
}
```

Further documentation can be found at <https://hexdocs.pm/jasper>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
