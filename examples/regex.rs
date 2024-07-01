use std::ops::RangeInclusive;

enum Class {
    Character(char),
    Any,
    Word,
    NotWord,
    Digit,
    NotDigit,
    Whitespace,
    NotWhitespace,
}

enum RangeItem {
    Char(char),
    Range(RangeInclusive<char>),
}

enum Range {
    Set(Vec<RangeItem>),
    NegatedSet(Vec<RangeItem>),
}

enum Group {
    Capturing(Vec<Token>),
    NamedCapturing(Vec<Token>),
    NonCapturing(Vec<Token>),
    NumericReference(usize),
    NamedReference(String),
}

enum Lookaround {
    PositiveLookahead(Vec<Token>),
    NegativeLookahead(Vec<Token>),
    PositiveLookbehind(Vec<Token>),
    NegativeLookbehind(Vec<Token>),
}

enum Quantifier {
    Option(Token),
    Plus(Token),
    Star(Token),
    Numbered(Token, Option<usize>, Option<usize>),
    Alternation(Vec<Token>, Vec<Token>),
}

enum Anchor {
    Start,
    End,
    Boundary,
    NotBoundary,
}

enum Token {
    Class(Class),
    RangeItem(RangeItem),
    Quantifier(Box<Quantifier>),
    Group(Box<Group>),
    Lookaround(Box<Lookaround>),
    Anchor(Anchor),
}

fn main() {
    let x = 2;
    println!("{x}");
    let x = "foo";
    println!("{x}");
}
// let x: impl Fn() = || vec;
// let args: Vec<String> = std::env::args().collect();
// if atty::is(atty::Stream::Stdin) {
//     let file_path = args.get(1).expect(
//         "
//     Expected file as first argument
//     usage:
//         cargo run --example regex <FILE> <PATTERN>
//     ",
//     );
//     let file = std::fs::read_to_string(file_path)?;
//     let regex_pattern = args.get(2).unwrap_or_else(|| {
//         panic!(
//             "
//     Expected regex pattern as second argument
//     usage:
//         cargo run --example regex {file_path} <PATTERN>
//     "
//         )
//     });
//     regex_matches(&file, regex_pattern)
// } else {
//     let mut file = String::new();
//     let regex_pattern = args.get(1).expect(
//         "
//     Expected regex pattern as first argument
//     usage:
//         cargo run --example regex <PATTERN>
//     ",
//     );
//     let _ = io::stdin().read_to_string(&mut file)?;
//     regex_matches(&file, regex_pattern)
// }
// }
