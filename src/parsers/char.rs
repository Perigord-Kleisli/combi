use crate::{
    choice,
    combinators::{branching::Branching, repeating::Repeating, sequential::Sequential},
    parser::{self, perr, pok, Consumption, ErrorItem, PFailure, PResult, PState, Parser},
    stream::Stream,
};

#[inline]
fn uncons_char_stream<S>(input: PState<'_, S>) -> PResult<'_, S, char>
where
    S: Stream<Item = char>,
{
    input.uncons(|&c| c == '\n')
}

#[inline]
fn char_satisfy<'input, S, F>(f: F) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
    F: Fn(&char) -> bool,
{
    parser::satisfy(f, |c| *c == '\n')
}

#[inline]
fn char_map<'input, S, F, T>(f: F) -> impl Parser<'input, S, T>
where
    S: Stream<Item = char>,
    F: Fn(&char) -> Option<T>,
{
    parser::satisfy_map(f, |c| *c == '\n')
}

// Parses a single character
#[inline]
pub fn char<'input, S>(c: char) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    parser::satisfy_eq(c, c == '\n')
}

/// Returns any single character
#[inline]
pub fn any_single<S>(input: PState<'_, S>) -> PResult<'_, S, char>
where
    S: Stream<Item = char>,
{
    uncons_char_stream(input)
}

/// Parses any single character in `s`
pub fn one_of<'input, S>(s: &'input str) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    move |input: PState<'input, S>| {
        let (c, input) = uncons_char_stream(input)?;
        if s.contains(c) {
            pok(c, input.set_consuming())
        } else {
            let expected = s.chars().map(ErrorItem::Token).collect();
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(c)),
                expected,
            )
        }
    }
}

/// Parses any single character not in `s`
/// it's a good idea to attach a `label` to this parser
pub fn none_of<'input, S>(s: &'input str) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    move |input: PState<'input, S>| {
        let (c, input) = uncons_char_stream(input)?;
        if !s.contains(c) {
            pok(c, input.set_consuming())
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(c)),
                vec![],
            )
        }
    }
}

/// Parses any single character except `c`
pub fn not_char<'input, S>(c: char) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    move |input| {
        let (x, xs) = uncons_char_stream(input)?;
        if x != c {
            pok(x, xs)
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(c)),
                vec![],
            )
        }
    }
}

/// Parses a sequence of characters provided by `s`
pub fn string<'input>(s: &'input str) -> impl Parser<'input, &'input str, &'input str> {
    move |input: PState<'input, &'input str>| {
        if let Some(s2) = input.input.get(..s.len()) {
            if s == s2 {
                let location = s.chars().fold(input.location, |accum, x| {
                    if x == '\n' {
                        accum.advance_line()
                    } else {
                        accum.advance_col()
                    }
                });

                pok(
                    s2,
                    PState {
                        input: &input.input[s.len()..],
                        location,
                        consumption: Consumption::Consuming,
                    },
                )
            } else {
                perr(
                    input.location,
                    Consumption::NonConsuming,
                    Some(ErrorItem::Tokens(s2.chars().collect())),
                    vec![ErrorItem::Tokens(s.chars().collect())],
                )
            }
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::EOF),
                vec![ErrorItem::Tokens(s.chars().collect())],
            )
        }
    }
}

/// Parses a single digit
#[inline]
pub fn digit<'input, S: Stream<Item = char>>(radix: u32) -> impl Parser<'input, S, char> {
    move |input| {
        char_satisfy(|x| x.is_digit(radix))
            .label("digit")
            .raw_parse(input)
    }
}

/// Parses a single ascii digit
#[inline]
pub fn ascii_digit<S: Stream<Item = char>>(input: PState<'_, S>) -> PResult<'_, S, char> {
    char_satisfy(|x| x.is_ascii_digit())
        .label("digit")
        .raw_parse(input)
}

/// Parses a single whitespace character
pub fn space_char<S: Stream<Item = char>>(state: PState<'_, S>) -> PResult<'_, S, char> {
    char_satisfy(|x| x.is_whitespace())
        .label("whitespace")
        .raw_parse(state)
}

/// Parses zero or more space characters
pub fn space<S>(input: PState<'_, S>) -> PResult<'_, S, ()>
where
    S: Stream<Item = char>,
{
    space_char.many().ignore().raw_parse(input)
}

/// Parses one or more space characters
pub fn space1<S>(input: PState<'_, S>) -> PResult<'_, S, ()>
where
    S: Stream<Item = char>,
{
    space_char.many().ignore().raw_parse(input)
}

pub trait Lexeme<'a, S, T>: Parser<'a, S, T> + Sequential<'a, S, T>
where
    S: Stream<Item = char>,
{
    ///Parses `self` and the whitespace after it
    fn lexeme(&self) -> impl Parser<'a, S, T> {
        self.seq_l(space)
    }
}

impl<'a, S, T, P> Lexeme<'a, S, T> for P
where
    P: Parser<'a, S, T>,
    S: Stream<Item = char>,
{
}

/// parses a string `s` and the whitespace after it
pub fn symbol(s: &str) -> impl Parser<'_, &'_ str, &'_ str> {
    move |input| string(s).lexeme().raw_parse(input)
}

pub fn eol<'input>(input: PState<'input, &'input str>) -> PResult<'input, &'input str, ()> {
    char('\n').ignore().or(string("\r\n").ignore()).raw_parse(input)
}

/// Parses an integer, possibly negative or not
pub fn int<S>(state: PState<'_, S>) -> PResult<'_, S, i64>
where
    S: Stream<Item = char>,
{
    (move |state| {
        let (x, state) = char('-').optional().raw_parse(state)?;
        let neg = x.is_some();

        let (x, state) = ascii_digit.many1().raw_parse(state)?;

        let mut n: i64 = 0;
        for (p, c) in x.iter().rev().enumerate() {
            n += (c.to_digit(10).unwrap() as i64) * (10_i64.pow(p as u32));
        }

        if neg {
            n *= -1;
        }

        Ok((n, state))
    })
    .label("Integer")
    .raw_parse(state)
}

pub fn float<S>(input: PState<'_, S>) -> PResult<'_, S, f64>
where
    S: Stream<Item = char>,
{
    (move |state| {
        let (x, state) = char('-').optional().raw_parse(state)?;
        let neg = x.is_some();

        let (x, state) = ascii_digit.or(char('.')).many1().raw_parse(state)?;

        let mut n: f64 = x.into_iter().collect::<String>().parse().unwrap();

        if neg {
            n *= -1.0;
        }

        Ok((n, state))
    })
    .label("Float")
    .raw_parse(input)
}

pub fn usize<S>(input: PState<'_, S>) -> PResult<'_, S, usize>
where
    S: Stream<Item = char>,
{
    lex_integer(10).map(|x| x as usize).raw_parse(input)
}

fn lex_esc_char<S>(input: PState<'_, S>) -> PResult<'_, S, char>
where
    S: Stream<Item = char>,
{
    let (x, input) = any_single.raw_parse(input)?;
    match x {
        'n' => Ok(('\n',  input.set_consuming())),
        'r' => Ok(('\r',  input.set_consuming())),
        't' => Ok(('\t',  input.set_consuming())),
        '\\' => Ok(('\\', input.set_consuming())),
        '\'' => Ok(('\'', input.set_consuming())),
        '\"' => Ok(('\"', input.set_consuming())),
        '\0' => Ok(('\0', input.set_consuming())),
        c => Err(PFailure {
            location: input.location,
            unexpected: Some(ErrorItem::Token(c)),
            consumption: Consumption::NonConsuming,
            expected: vec![],
        }),
    }
}

fn lex_base_char<S>(input: PState<'_, S>) -> PResult<'_, S, u32>
where
    S: Stream<Item = char>,
{
    let (x, input) = any_single(input)?;
    match x {
        'o' | 'O' => Ok((8, input.set_consuming())),
        'x' | 'X' => Ok((16, input.set_consuming())),
        c => Err(PFailure {
            location: input.location,
            unexpected: Some(ErrorItem::Token(c)),
            consumption: Consumption::NonConsuming,
            expected: vec![],
        }),
    }
}

fn lex_digits<'a, S>(base: u32) -> impl Parser<'a, S, Vec<u32>>
where
    S: Stream<Item = char>,
{
    move |input: PState<'a, S>| char_map(|c| c.to_digit(base)).many1().raw_parse(input)
}

pub fn lex_integer<'a, S>(base: u32) -> impl Parser<'a, S, u32>
where
    S: Stream<Item = char>,
{
    move |input: PState<'a, S>| {
        let (n, input) = lex_digits(base).raw_parse(input)?;
        let n = n.into_iter().fold(0, |accum, x| accum * base + x);
        Ok((n, input.set_consuming()))
    }
}

fn lex_numeric<S>(input: PState<'_, S>) -> PResult<'_, S, char>
where
    S: Stream<Item = char>,
{
    let (base, input) = lex_base_char.or_pure(10).raw_parse(input)?;
    let (n, input) = lex_integer(base).raw_parse(input)?;
    match char::from_u32(n) {
        Some(n) => Ok((n, input.set_consuming())),
        None => Err(PFailure {
            location: input.location,
            unexpected: Some(ErrorItem::Tokens(n.to_string().chars().collect())),
            consumption: input.consumption,
            expected: vec![],
        }),
    }
}
fn lex_char_e<S>(input: PState<'_, S>) -> PResult<'_, S, (char, bool)>
where
    S: Stream<Item = char>,
{
    let (x, input) = any_single.raw_parse(input)?;
    if x == '\\' {
        let (x, input) =
            choice![lex_esc_char.attempt(), lex_numeric].set_consuming().raw_parse(input)?;
        Ok(((x, true), input.set_consuming()))
    } else {
        Ok(((x, false), input.set_consuming()))
    }
}

pub fn char_literal<S>(input: PState<'_, S>) -> PResult<'_, S, char>
where
    S: Stream<Item = char>,
{
    (move |input| {
        let ((r, _), input) = lex_char_e(input)?;
        Ok((r, input.set_consuming()))
    })
    .label("Char Literal")
    .raw_parse(input)
}

// /// Regex character classes
// pub mod class {
//     use crate::{
//         defs::{PResult, PState, Reason, Stream},
//         parsers::char::any_single,
//     };
//
//     pub fn word<S>(input: PState<'_, S>) -> PResult<'_, S, char>
//     where
//         S: Stream<Item = char>,
//     {
//         let (x, input) = any_single(input)?;
//         if x.is_alphanumeric() || x == '_' {
//             Ok((x, input))
//         } else {
//             Err((input.location, Reason::expecteds("word character")))
//         }
//     }
//     pub fn not_word<S>(input: PState<'_, S>) -> PResult<'_, S, char>
//     where
//         S: Stream<Item = char>,
//     {
//         let (x, input) = any_single(input)?;
//         if !x.is_alphanumeric() && x != '_' {
//             Ok((x, input))
//         } else {
//             Err((input.location, Reason::expecteds("not word character")))
//         }
//     }
//
//     pub fn digit<S>(input: PState<'_, S>) -> PResult<'_, S, char>
//     where
//         S: Stream<Item = char>,
//     {
//         let (x, input) = any_single(input)?;
//         if x.is_ascii_digit() {
//             Ok((x, input))
//         } else {
//             Err((input.location, Reason::expecteds("digit character")))
//         }
//     }
//
//     pub fn not_digit<S>(input: PState<'_, S>) -> PResult<'_, S, char>
//     where
//         S: Stream<Item = char>,
//     {
//         let (x, input) = any_single(input)?;
//         if !x.is_ascii_digit() {
//             Ok((x, input))
//         } else {
//             Err((input.location, Reason::expecteds("digit character")))
//         }
//     }
//
//     pub fn space<S>(input: PState<'_, S>) -> PResult<'_, S, char>
//     where
//         S: Stream<Item = char>,
//     {
//         let (x, input) = any_single(input)?;
//         if x.is_whitespace() {
//             Ok((x, input))
//         } else {
//             Err((input.location, Reason::expecteds("digit character")))
//         }
//     }
//
//     pub fn not_space<S>(input: PState<'_, S>) -> PResult<'_, S, char>
//     where
//         S: Stream<Item = char>,
//     {
//         let (x, input) = any_single(input)?;
//         if !x.is_whitespace() {
//             Ok((x, input))
//         } else {
//             Err((input.location, Reason::expecteds("digit character")))
//         }
//     }
//
//     pub fn word_boundary<S>(input: PState<'_, S>) -> PResult<'_, S, char>
//     where
//         S: Stream<Item = char>,
//     {
//         let (x, input) = word(input)?;
//         let (_, _) = not_word(input)?;
//         Ok((x, input))
//     }
//
//     pub fn not_word_boundary<S>(input: PState<'_, S>) -> PResult<'_, S, char>
//     where
//         S: Stream<Item = char>,
//     {
//         let (_, input) = not_word(input)?;
//         let (x, input) = word(input)?;
//         Ok((x, input))
//     }
// }
