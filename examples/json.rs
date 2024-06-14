#![feature(iter_intersperse)]

use combi::defs::{
    pretty_print_error, Branching, PResult, PState, Parser, Repeating, Sequential, Stream,
};
use combi::parsers::char::*;
use std::collections::HashMap;
use std::io::{self, Read};

#[derive(Debug, Clone)]
pub enum Json {
    Object(HashMap<String, Json>),
    Array(Vec<Json>),
    Num(f64),
    String(String),
    Bool(bool),
    Null,
}

fn bool_p<S>(input: PState<'_, S>) -> PResult<'_, S, bool>
where
    S: Stream<Item = char>,
{
    symbol("true")
        .fconst(true)
        .or(symbol("false").fconst(false))
        .parse(input)
}

fn bool<S>(input: PState<'_, S>) -> PResult<'_, S, Json>
where
    S: Stream<Item = char>,
{
    bool_p.map(Json::Bool).parse(input)
}

fn null<S>(input: PState<'_, S>) -> PResult<'_, S, Json>
where
    S: Stream<Item = char>,
{
    symbol("null").fconst(Json::Null).parse(input)
}

fn num<S>(input: PState<'_, S>) -> PResult<'_, S, Json>
where
    S: Stream<Item = char>,
{
    float.map(Json::Num).parse(input)
}

fn string_literal<S>(input: PState<'_, S>) -> PResult<'_, S, String>
where
    S: Stream<Item = char>,
{
    char('"')
        .ignore_left(&char_literal.many_till(&char('"')))
        .map(|x| x.into_iter().collect())
        .parse(input)
}

fn json_string<S>(input: PState<'_, S>) -> PResult<'_, S, Json>
where
    S: Stream<Item = char>,
{
    string_literal.map(Json::String).parse(input)
}

fn json_array<S>(input: PState<'_, S>) -> PResult<'_, S, Json>
where
    S: Stream<Item = char>,
{
    symbol("[")
        .ignore_left(&json_parser.lexeme().sep_by(&symbol(",")))
        .ignore_right(char(']'))
        .map(Json::Array)
        .parse(input)
}

fn json_object<S>(input: PState<'_, S>) -> PResult<'_, S, Json>
where
    S: Stream<Item = char>,
{
    fn json_map_item<S>(input: PState<'_, S>) -> PResult<'_, S, (String, Json)>
    where
        S: Stream<Item = char>,
    {
        string_literal
            .lexeme()
            .ignore_right(symbol(":"))
            .and_then(json_parser)
            .parse(input)
    }

    let (x, input) = symbol("{")
        .ignore_left(&json_map_item.lexeme().sep_by(&symbol(",")))
        .ignore_right(symbol("}"))
        .parse(input)?;

    Ok((Json::Object(x.into_iter().collect()), input))
}

fn json_parser<S>(input: PState<'_, S>) -> PResult<'_, S, Json>
where
    S: Stream<Item = char>,
{
    json_string
        .or(json_object)
        .or(null)
        .or(bool)
        .or(num)
        .or(json_array)
        .parse(input)
}

fn pretty_print_json(indent: usize, json: &Json) -> String {
    let mut out = "  ".repeat(indent);
    match json {
        Json::Object(object) => {
            if object.is_empty() {
                out.push_str("{}");
                return out;
            }
            out.push_str("{\n");
            for (k, v) in object.iter() {
                let s = pretty_print_json(indent + 1, v);

                if atty::is(atty::Stream::Stdout) {
                    out.push_str(&format!(
                        "{}\x1b[34m\"{k}\"\x1b[0m: {}",
                        "  ".repeat(indent + 1),
                        s.trim_start()
                    ));
                } else {
                    out.push_str(&format!(
                        "{}\"{k}\": {}",
                        "  ".repeat(indent + 1),
                        s.trim_start()
                    ));
                }
                out.push_str(",\n");
            }
            out.pop();
            out.pop();
            out.push('\n');
            out.push_str(&"  ".repeat(indent));
            out.push('}');
        }
        Json::Array(arr) => {
            if arr.is_empty() {
                out.push_str("[]");
                return out;
            }
            let arr: Vec<String> = arr
                .iter()
                .map(|x| pretty_print_json(indent + 1, x))
                .collect();
            let lengths: usize = arr.iter().map(|x| x.trim_start().len()).sum();
            if lengths + indent >= 40 {
                out.push_str("[\n");

                for i in arr {
                    out.push_str(&i);
                    out.push_str(",\n");
                }
                out.pop();
                out.pop();
                out.push('\n');

                out.push_str(&"  ".repeat(indent));
                out.push(']');
            } else {
                out.push_str(&format!(
                    "[{}]",
                    arr.iter()
                        .map(|x| x.trim_start())
                        .intersperse(",")
                        .fold(String::new(), |acc, x| acc + x)
                ));
            }
        }
        Json::Num(n) => out.push_str(&n.to_string()),
        Json::String(s) => {
            if atty::is(atty::Stream::Stdout) {
                out.push_str(&format!("\x1b[32m\"{s}\"\x1b[0m"))
            } else {
                out.push_str(&format!("\"{s}\""))
            }
        }
        Json::Bool(b) => out.push_str(&format!("{b}")),
        Json::Null => out.push_str("null"),
    }
    out
}

fn key_json(file_path: &str, file: &str, keys: &[String]) {
    match json_parser
        .lexeme()
        .exhaustive()
        .run_parser(file_path, file)
    {
        Ok((mut json, _)) => {
            let mut indexed = String::from("");
            for key in keys {
                json = match json {
                    Json::Object(mut ob) => {
                        indexed.push_str(&format!(".\"{key}\""));
                        ob.remove(key)
                            .unwrap_or_else(|| panic!("key: '{indexed}' does not exist"))
                    }
                    Json::Array(mut arr) => {
                        let i: usize = key.parse().unwrap_or_else(|_| {
                            panic!("Array cannot be indexed by non-integer: '{key}'")
                        });
                        indexed.push_str(&format!(".{key}"));
                        if i >= arr.len() {
                            panic!("key: '{indexed}' does not exist")
                        }
                        arr.swap_remove(i)
                    }
                    _ => panic!("no key {key} in json:\n{json:#?}"),
                };
            }
            println!("{}", pretty_print_json(0, &json));
        }
        Err(e) => println!("{}", pretty_print_error(&e)),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if atty::is(atty::Stream::Stdin) {
        let file_path = args.get(1).expect(
            "
        Expected json file as first argument
        usage:
            cargo run --example json <FILE> [..keys]
        ",
        );
        let file = std::fs::read_to_string(file_path)?;
        let keys = args.get(2..).unwrap_or(&[]);
        key_json(file_path, &file, keys);
    } else {
        let mut stdin = io::stdin();
        let mut file = String::new();
        stdin.read_to_string(&mut file)?;
        let keys = args.get(1..).unwrap_or(&[]);
        key_json("<STDIN>", &file, keys);
    }
    Ok(())
}
