# jasper

Credit to [kwando](https://github.com/kwando) for the `decoder` and `to_json` functions. If the querying part of this library is not required, I recommend using their package [`json_value`](https://hexdocs.pm/json_value) instead.

[![Package Version](https://img.shields.io/hexpm/v/jasper)](https://hex.pm/packages/jasper)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/jasper/)

```sh
gleam add jasper
```
```gleam
import gleam/io
import jasper.{String, Root, Key, Index}

pub fn main() {
  let assert Ok(json) = jasper.parse("{ \"foo\": [1, true, \"hi\"] }")
  let assert Ok(String(str)) = jasper.query(json, Root |> Key("foo") |> Index(2))
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
