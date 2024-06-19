use std::{
    collections::HashMap,
    io::{self, Read},
};

use combi::{
    choice,
    parser::{
        branching::Branching, pok, repeating::Repeating, sequential::Sequential, PResult, PState,
        Parser,
    },
    parsers::char::{char, char_literal, float, symbol, Lexeme},
    stream::Stream,
};

// #![feature(iter_intersperse)]
//
// use combi::defs::{
//     pretty_print_error, Branching, PResult, PState, Parser, Repeating, Sequential, Stream,
// };
// use combi::parsers::char::*;
// use std::collections::HashMap;
// use std::io::{self, Read};
//

type PInput<'input> = PState<'input, &'input str>;
type POutput<'input, T> = PResult<'input, &'input str, T>;

#[derive(Debug, Clone)]
pub enum Json {
    Object(HashMap<String, Json>),
    Array(Vec<Json>),
    Num(f64),
    String(String),
    Bool(bool),
    Null,
}

fn bool(input: PInput<'_>) -> POutput<'_, Json> {
    symbol("true")
        .replace(Json::Bool(true))
        .or(symbol("false").replace(Json::Bool(false)))
        .raw_parse(input)
}

fn null(input: PInput<'_>) -> POutput<'_, Json> {
    symbol("null").replace(Json::Null).raw_parse(input)
}

fn num(input: PInput<'_>) -> POutput<'_, Json> {
    float.map(Json::Num).raw_parse(input)
}

fn string_literal(input: PInput<'_>) -> POutput<'_, String> {
    char('"')
        .seq_r(char_literal.many_till(char('"')))
        .map(|x| x.into_iter().collect())
        .raw_parse(input)
}

fn string(input: PInput<'_>) -> POutput<'_, Json> {
    string_literal.map(Json::String).raw_parse(input)
}

fn array(input: PInput<'_>) -> POutput<'_, Json> {
    let (_, input) = symbol("[").raw_parse(input)?;
    let (xs, input) = json_parser.sep_by(symbol(",")).raw_parse(input)?;
    let (_, input) = symbol("]").raw_parse(input)?;
    pok(Json::Array(xs), input)
}

fn object(input: PInput<'_>) -> POutput<'_, Json> {
    let (_, input) = symbol("{").raw_parse(input)?;

    fn pair(input: PInput<'_>) -> POutput<'_, (String, Json)> {
        let (s, input) = string_literal.raw_parse(input)?;
        let (_, input) = symbol(":").raw_parse(input)?;
        let (x, input) = json_parser.lexeme().raw_parse(input)?;
        pok((s, x), input)
    }

    let (map,input) = pair.sep_by(symbol(",")).raw_parse(input)?;

    // let (map, input) = pair.sep_by(symbol(",")).parse(input)?;

    let (_, input) = symbol("}").raw_parse(input)?;
    pok(Json::Object(HashMap::from_iter(map)), input)
}

fn json_parser(input: PInput<'_>) -> POutput<'_, Json> {
    choice![null, bool, num, string, array, object].raw_parse(input)
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
                out.push('[');
                let mut arr_iter = arr.iter();
                if let Some(x) = arr_iter.next() {
                    out.push_str(x);
                }
                for i in arr_iter {
                    out.push(',');
                    out.push_str(i);
                }
                out.push(']');
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
        Ok(mut json) => {
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
        Err(e) => println!("{:?}", &e),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    // let args: Vec<String> = vec!["".to_string(), String::from("/home/truff/.local/src/combi/examples/sample.json")];

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
