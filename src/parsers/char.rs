use std::ops::RangeBounds;

use crate::{
    choice,
    combinators::{branching::Branching, repeating::Repeating, sequential::Sequential},
    error::ErrorItem,
    parser::{self, Parser},
    state::{perr, pok, Consumption, PResult, PState, ParserResult, SecondStagePResult},
    stream::Stream,
};

#[inline]
fn uncons_char_stream<St, S>(input: PState<'_, St, S>) -> SecondStagePResult<'_, St, S, char>
where
    S: Stream<Item = char>,
{
    input.uncons(|&c| c == '\n')
}

#[inline]
fn char_satisfy<'input, St, S, F>(f: F) -> impl Parser<'input, St, S, char>
where
    S: Stream<Item = char>,
    F: Fn(&char) -> bool,
{
    parser::satisfy(f, |c| *c == '\n')
}

#[inline]
fn char_map<'input, St, S, F, T>(f: F) -> impl Parser<'input, St, S, T>
where
    S: Stream<Item = char>,
    F: Fn(&char) -> Option<T>,
{
    parser::satisfy_map(f, |c| *c == '\n')
}

// Parses a single character
#[inline]
pub fn char<'input, St, S>(c: char) -> impl Parser<'input, St, S, char>
where
    S: Stream<Item = char>,
{
    parser::satisfy_eq(c, c == '\n')
}

/// Returns any single character
#[inline]
pub fn any_single<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
where
    S: Stream<Item = char>,
{
    uncons_char_stream(input).to_first_stage()
}

/// Parses any single character in `s`
pub fn one_of<'input, St, S>(s: &'input str) -> impl Parser<'input, St, S, char>
where
    S: Stream<Item = char>,
{
    move |input: PState<'input, St, S>| {
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
pub fn none_of<'input, St, S>(s: &'input str) -> impl Parser<'input, St, S, char>
where
    S: Stream<Item = char>,
{
    move |input: PState<'input, St, S>| {
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
pub fn not_char<'input, St, S>(c: char) -> impl Parser<'input, St, S, char>
where
    S: Stream<Item = char>,
    St: Clone,
{
    move |input: PState<'input, St, S>| {
        let (x, xs) = uncons_char_stream(input.clone())?;
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
pub fn string<'input, St>(s: &'input str) -> impl Parser<'input, St, &'input str, &'input str> {
    move |input: PState<'input, St, &'input str>| {
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
                        user_state: input.user_state,
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
pub fn digit<'input, St, S: Stream<Item = char>>(radix: u32) -> impl Parser<'input, St, S, char> {
    move |input| {
        char_satisfy(|x| x.is_digit(radix))
            .label("digit")
            .raw_parse(input)
    }
}

/// Parses a single ascii digit
#[inline]
pub fn ascii_digit<St, S: Stream<Item = char>>(
    input: PState<'_, St, S>,
) -> PResult<'_, St, S, char> {
    char_satisfy(|x| x.is_ascii_digit())
        .label("digit")
        .raw_parse(input)
}

/// Parses a single whitespace character
pub fn space_char<St, S: Stream<Item = char>>(
    state: PState<'_, St, S>,
) -> PResult<'_, St, S, char> {
    char_satisfy(|x| x.is_whitespace())
        .label("whitespace")
        .raw_parse(state)
}

/// Parses zero or more space characters
pub fn space<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, ()>
where
    S: Stream<Item = char>,
    St: Clone,
{
    space_char.many().ignore().raw_parse(input)
}

/// Parses one or more space characters
pub fn space1<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, ()>
where
    S: Stream<Item = char>,
    St: Clone,
{
    space_char.many().ignore().raw_parse(input)
}

pub trait Lexeme<'a, St, S, T>: Parser<'a, St, S, T> + Sequential<'a, St, S, T>
where
    S: Stream<Item = char>,
    St: Clone,
{
    ///Parses `self` and the whitespace after it
    fn lexeme(&self) -> impl Parser<'a, St, S, T> {
        self.seq_l(space)
    }
}

impl<'a, St, S, T, P> Lexeme<'a, St, S, T> for P
where
    P: Parser<'a, St, S, T>,
    S: Stream<Item = char>,
    St: Clone,
{
}

/// parses a string `s` and the whitespace after it
pub fn symbol<St>(s: &str) -> impl Parser<'_, St, &'_ str, &'_ str>
where
    St: Clone,
{
    move |input| string(s).lexeme().raw_parse(input)
}

pub fn eol<'input, St>(
    input: PState<'input, St, &'input str>,
) -> PResult<'input, St, &'input str, ()>
where
    St: Clone,
{
    char('\n')
        .ignore()
        .or(string("\r\n").ignore())
        .raw_parse(input)
}

/// Parses an integer, possibly negative or not
pub fn int<St, S>(state: PState<'_, St, S>) -> PResult<'_, St, S, i64>
where
    S: Stream<Item = char>,
    St: Clone,
{
    (move |state| {
        let (x, state) = char('-').optional().raw_parse(state)?;
        let neg = x.is_some();

        let (x, state) = ascii_digit.some().raw_parse(state)?;

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

pub fn float<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, f64>
where
    S: Stream<Item = char>,
    St: Clone,
{
    (move |state| {
        let (x, state) = char('-').optional().raw_parse(state)?;
        let neg = x.is_some();

        let (x, state) = ascii_digit.or(char('.')).some().raw_parse(state)?;

        let mut n: f64 = x.into_iter().collect::<String>().parse().unwrap();

        if neg {
            n *= -1.0;
        }

        Ok((n, state))
    })
    .label("Float")
    .raw_parse(input)
}

pub fn usize<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, usize>
where
    S: Stream<Item = char>,
    St: Clone,
{
    lex_integer(10).map(|x| x as usize).raw_parse(input)
}

fn lex_esc_char<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
where
    S: Stream<Item = char>,
{
    let (x, input) = any_single.raw_parse(input)?;
    match x {
        'n' => Ok(('\n', input.set_consuming())),
        'r' => Ok(('\r', input.set_consuming())),
        't' => Ok(('\t', input.set_consuming())),
        '\\' => Ok(('\\', input.set_consuming())),
        '\'' => Ok(('\'', input.set_consuming())),
        '\"' => Ok(('\"', input.set_consuming())),
        '\0' => Ok(('\0', input.set_consuming())),
        c => perr(
            input.location,
            Consumption::NonConsuming,
            Some(ErrorItem::Token(c)),
            vec![],
        ),
    }
}

fn lex_base_char<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, u32>
where
    S: Stream<Item = char>,
{
    let (x, input) = any_single(input)?;
    match x {
        'o' | 'O' => Ok((8, input.set_consuming())),
        'x' | 'X' => Ok((16, input.set_consuming())),
        c => perr(
            input.location,
            Consumption::NonConsuming,
            Some(ErrorItem::Token(c)),
            vec![],
        ),
    }
}

fn lex_digits<'a, St, S>(base: u32) -> impl Parser<'a, St, S, Vec<u32>>
where
    S: Stream<Item = char>,
    St: Clone,
{
    move |input: PState<'a, St, S>| char_map(|c| c.to_digit(base)).some().raw_parse(input)
}

pub fn lex_integer<'a, St, S>(base: u32) -> impl Parser<'a, St, S, u32>
where
    S: Stream<Item = char>,
    St: Clone,
{
    move |input: PState<'a, St, S>| {
        let (n, input) = lex_digits(base).raw_parse(input)?;
        let n = n.into_iter().fold(0, |accum, x| accum * base + x);
        Ok((n, input.set_consuming()))
    }
}

fn lex_numeric<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
where
    S: Stream<Item = char>,
    St: Clone,
{
    let (base, input) = lex_base_char.or_pure(10).raw_parse(input)?;
    let (n, input) = lex_integer(base).raw_parse(input)?;
    match char::from_u32(n) {
        Some(n) => Ok((n, input.set_consuming())),
        None => perr(
            input.location,
            input.consumption,
            Some(ErrorItem::Tokens(n.to_string().chars().collect())),
            vec![],
        ),
    }
}

pub fn range<'input, St, S, Range: RangeBounds<char>>(
    range: Range,
) -> impl Parser<'input, St, S, char>
where
    S: Stream<Item = char>,
{
    move |input| {
        let (c, input) = uncons_char_stream(input)?;
        if range.contains(&c) {
            pok(c, input)
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

pub fn not_range<'input, St, S, Range: RangeBounds<char>>(
    range: Range,
) -> impl Parser<'input, St, S, char>
where
    S: Stream<Item = char>,
{
    move |input| {
        let (c, input) = uncons_char_stream(input)?;
        if !range.contains(&c) {
            pok(c, input)
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

fn lex_char_e<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, (char, bool)>
where
    S: Stream<Item = char>,
    St: Clone,
{
    let (x, input) = any_single.raw_parse(input)?;
    if x == '\\' {
        let (x, input) = choice![lex_esc_char.attempt(), lex_numeric]
            .set_consuming()
            .raw_parse(input)?;
        Ok(((x, true), input.set_consuming()))
    } else {
        Ok(((x, false), input.set_consuming()))
    }
}

pub fn char_literal<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
where
    S: Stream<Item = char>,
    St: std::clone::Clone,
{
    (move |input| {
        let ((r, _), input) = lex_char_e(input)?;
        Ok((r, input.set_consuming()))
    })
    .label("Char Literal")
    .raw_parse(input)
}

pub fn any_with_escapes<St, S>(s: &str) -> impl Parser<'_, St, S, char>
where
    S: Stream<Item = char>,
{
    move |input| {
        let (x, input) = none_of(s).parse(input)?;
        if x == '\\' {
            let (x, input) = any_single.parse(input)?;
            match x {
                'n' => pok('\n', input),
                'r' => pok('\r', input),
                't' => pok('\t', input),
                '\\' => pok('\\', input),
                '\'' => pok('\'', input),
                '\"' => pok('\"', input),
                '\0' => pok('\0', input),
                x => {
                    if s.contains(x) {
                        pok(x, input)
                    } else {
                        perr(
                            input.location,
                            Consumption::Consuming,
                            Some(ErrorItem::Token(x)),
                            vec![],
                        )
                    }
                }
            }
        } else {
            pok(x, input)
        }
    }
}

/// Regex character classes
pub mod class {
    use crate::{
        error::ErrorItem,
        parser::{Parser, Stream},
        parsers::char::any_single,
        state::{perr, pok, Consumption, PResult, PState},
    };

    pub fn word<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single.parse(input)?;
        if x.is_alphanumeric() || x == '_' {
            pok(x, input)
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(x)),
                vec![],
            )
        }
    }
    pub fn not_word<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single.parse(input)?;
        if !x.is_alphanumeric() && x != '_' {
            pok(x, input)
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(x)),
                vec![],
            )
        }
    }

    pub fn digit<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single.parse(input)?;
        if x.is_ascii_digit() {
            pok(x, input)
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(x)),
                vec![],
            )
        }
    }

    pub fn not_digit<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single.parse(input)?;
        if !x.is_ascii_digit() {
            pok(x, input)
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(x)),
                vec![],
            )
        }
    }

    pub fn space<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single.parse(input)?;
        if x.is_whitespace() {
            pok(x, input)
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(x)),
                vec![],
            )
        }
    }

    pub fn not_space<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single.parse(input)?;
        if !x.is_whitespace() {
            pok(x, input)
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(x)),
                vec![],
            )
        }
    }

    pub fn word_boundary<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
    where
        S: Stream<Item = char>,
        St: Clone,
    {
        let (x, input) = word.parse(input)?;
        let (_, _) = not_word.parse(input.clone())?;
        pok(x, input)
    }

    pub fn not_word_boundary<St, S>(input: PState<'_, St, S>) -> PResult<'_, St, S, char>
    where
        S: Stream<Item = char>,
    {
        let (_, input) = not_word.parse(input)?;
        let (x, input) = word.parse(input)?;
        pok(x, input)
    }
}
