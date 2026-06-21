# kaappi-json

JSON parser and serializer for [Kaappi Scheme](https://github.com/kaappi/kaappi).

Pure Scheme — no C dependencies, no build step.

## Usage

```bash
kaappi --lib-path /path/to/kaappi-json/lib your-script.scm
```

```scheme
(import (kaappi json))

;; Parse
(json-read-string "{\"name\": \"Alice\", \"age\": 30}")
;; => (("name" . "Alice") ("age" . 30))

(json-read-string "[1, 2, 3]")
;; => (1 2 3)

;; Serialize
(json-write-string '(("name" . "Alice") ("age" . 30)))
;; => "{\"name\":\"Alice\",\"age\":30}"

(json-write-string '(1 2 3))
;; => "[1,2,3]"

;; Read from / write to ports
(json-read port)
(json-write value port)
```

## Type Mapping

| JSON | Scheme | Example |
|---|---|---|
| object | alist | `(("key" . "value"))` |
| array | list | `(1 2 3)` |
| string | string | `"hello"` |
| number (int) | exact integer | `42` |
| number (float) | inexact number | `3.14` |
| true | `#t` | |
| false | `#f` | |
| null | `'null` symbol | `(json-null? x)` to test |

Vectors are written as JSON arrays: `#(1 2 3)` → `[1,2,3]`.

## API

| Procedure | Description |
|---|---|
| `(json-read [port])` | Parse JSON from port (default: current-input-port) |
| `(json-read-string str)` | Parse JSON from a string |
| `(json-write val [port])` | Write JSON to port (default: current-output-port) |
| `(json-write-string val)` | Write JSON to a string |
| `(json-null)` | The null value (`'null` symbol) |
| `(json-null? val)` | Test for null |

## Features

- Full JSON spec compliance (RFC 8259)
- Unicode escape sequences (`\uXXXX`) including surrogate pairs
- All escape sequences (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`)
- Scientific notation (`1.5e-3`)
- Round-trip safe (parse then serialize preserves structure)

## Tests

```bash
kaappi --lib-path lib tests/test-json.scm
```

## License

MIT
