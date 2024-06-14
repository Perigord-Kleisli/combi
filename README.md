# Combi
Combi is a parsec-inspired Rust parser **combi**nator library. 
It can parse any type implementing `Stream` trait, and has built-in implementations for `&str` and `&[u8]`.

# Features
- Built in error-handling

# Example programs

## JSON Parser
### Usage:

`cargo run --example json <FILE> [..keys]`

The file can also be received from piping:

`cargo run --example json [..keys] < <FILE>` or `<program> | cargo run --example json [..keys]`


### Example:
```bash
$ echo '{"a": {"c": 2}, "b": [2,3] }' | cargo run --example json a
{
  "c": 2
}

$ echo '{"a": {"c": 2}, "b": [2,3] }' | cargo run --example json a c
2

$ echo '{"a": {"c": 2}, "b": [2,3] }' | cargo run --example json b 1
3
```

## RegEx Engine
### Usage:
`cargo run --example regex <FILE> <PATTERN>`

The file can also be received from piping:

`cargo run --example regex <PATTERN> < <FILE>` or `<program> | cargo run --example regex <PATTERN>`

### Example:

""
```bash
$ echo "foobar\nbaz\nfoo" | cargo run --example regex 'foo'
foobar
foo

$ cargo run --example regex '[a-zA-Z0-9._%-]+@[a-zA-Z0-9-]+\.[a-zA-Z]{2,4}' <<EOF
validmail@email.com
invalidEmail.com
validmail_2@email.com
invalidmail_2&@a.com
invalidmail_3@com
VAL%idmail_2@email.com
EOF
validmail@email.com
validmail_2@email.com
VAL%idmail_2@email.com
```

# TODO
- [ ] Add a difference between Errors that consume input vs those that don't, and generally improving parser errors.
- [ ] Rust-style parser error message pretty printing 
- [ ] Regex parser macro
- [ ] Add unit tests
- [ ] Documentation
- [ ] Optimization
