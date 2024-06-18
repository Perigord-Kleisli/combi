use std::{borrow::Borrow, fmt::Debug, path::Path, rc::Rc};

pub use crate::combinators::branching;
pub use crate::combinators::repeating;
pub use crate::combinators::sequential;
pub use crate::stream::Stream;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLoc<'file> {
    pub col: usize,
    pub line: usize,
    pub file: &'file Path,
}

impl<'file> SourceLoc<'file> {
    #[inline]
    pub fn advance_col(self) -> Self {
        SourceLoc {
            col: self.col + 1,
            line: self.line,
            file: self.file,
        }
    }

    #[inline]
    pub fn advance_line(self) -> Self {
        SourceLoc {
            col: 0,
            line: self.line + 1,
            file: self.file,
        }
    }
}

impl<'file> PartialOrd for SourceLoc<'file> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'file> Ord for SourceLoc<'file> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.line.cmp(&other.line) {
            std::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        self.col.cmp(&other.col)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PState<'input, S> {
    pub input: S,
    pub location: SourceLoc<'input>,
    pub consumption: Consumption,
}

impl<'input, S: Stream> PState<'input, S> {
    fn new(file: &'input str, input: S) -> Self {
        PState {
            input,
            location: SourceLoc {
                col: 0,
                line: 1,
                file: std::path::Path::new(file),
            },
            consumption: Consumption::NonConsuming,
        }
    }

    pub fn uncons<P>(self, is_newline: P) -> PResult<'input, S, S::Item>
    where
        P: Fn(&S::Item) -> bool,
    {
        match self.input.uncons() {
            None => perr(
                self.location.advance_col(),
                Consumption::NonConsuming,
                Some(ErrorItem::EOF),
                vec![],
            ),
            Some((x, xs)) => {
                if is_newline(&x) {
                    pok(
                        x,
                        PState {
                            input: xs,
                            location: self.location.advance_line(),
                            consumption: Consumption::Consuming,
                        },
                    )
                } else {
                    pok(
                        x,
                        PState {
                            input: xs,
                            location: self.location.advance_col(),
                            consumption: Consumption::Consuming,
                        },
                    )
                }
            }
        }
    }

    #[inline]
    pub fn has_consumed(&self) -> bool {
        match self.consumption {
            Consumption::Consuming => true,
            Consumption::NonConsuming => false,
        }
    }

    #[inline]
    pub fn has_not_consumed(&self) -> bool {
        match self.consumption {
            Consumption::Consuming => false,
            Consumption::NonConsuming => true,
        }
    }

    pub fn set_consumption(self, consumption: Consumption) -> Self {
        PState {
            input: self.input,
            location: self.location,
            consumption,
        }
    }

    #[inline]
    pub fn set_consuming(self) -> Self {
        PState {
            input: self.input,
            location: self.location,
            consumption: Consumption::Consuming,
        }
    }

    #[inline]
    pub fn set_nonconsuming(self) -> Self {
        PState {
            input: self.input,
            location: self.location,
            consumption: Consumption::NonConsuming,
        }
    }
}

#[derive(Debug)]
pub enum ErrorItem<Item> {
    Tokens(Vec<Item>),
    Token(Item),
    Branches(Vec<ErrorItem<Item>>),
    Label(String),
    Custom(String),
    EOF,
}

impl<Item> ErrorItem<Item> {
    pub fn merge(self, e2: Self) -> Self {
        if let ErrorItem::Branches(mut branches) = self {
            match e2 {
                ErrorItem::Branches(mut b) => branches.append(&mut b),
                e => branches.push(e),
            }
            ErrorItem::Branches(branches)
        } else if let ErrorItem::Branches(mut branches) = e2 {
            branches.push(self);
            ErrorItem::Branches(branches)
        } else {
            ErrorItem::Branches(vec![self, e2])
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Consumption {
    Consuming,
    NonConsuming,
}

impl Consumption {
    #[inline]
    pub fn merge(self, b: Consumption) -> Self {
        match self {
            Consumption::Consuming => Consumption::Consuming,
            _ => b,
        }
    }

    #[inline]
    pub fn is_consuming(self) -> bool {
        match self {
            Consumption::Consuming => true,
            Consumption::NonConsuming => false,
        }
    }

    #[inline]
    pub fn is_nonconsuming(self) -> bool {
        match self {
            Consumption::Consuming => false,
            Consumption::NonConsuming => true,
        }
    }
}

type PSuccess<'input, S, T> = (T, PState<'input, S>);

#[derive(Debug)]
pub struct PFailure<'input, Item> {
    pub location: SourceLoc<'input>,
    pub unexpected: Option<ErrorItem<Item>>,
    pub consumption: Consumption,
    pub expected: Vec<ErrorItem<Item>>,
}

pub type PResult<'input, S, T> =
    Result<PSuccess<'input, S, T>, PFailure<'input, <S as Stream>::Item>>;

#[inline]
pub fn pok<S: Stream, T>(output: T, state: PState<'_, S>) -> PResult<'_, S, T> {
    Ok((output, state))
}

#[inline]
pub fn perr<S: Stream, T>(
    location: SourceLoc<'_>,
    consumption: Consumption,
    unexpected: Option<ErrorItem<S::Item>>,
    expected: Vec<ErrorItem<S::Item>>,
) -> PResult<'_, S, T> {
    Err(PFailure {
        location,
        consumption,
        unexpected,
        expected,
    })
}

/// A general trait for parsers
pub trait Parser<'a, S, T>
where
    S: Stream,
{
    /// A direct implementation intended to be ran by other parsers.
    /// If intended to be ran directly by the user, instead use `run_parse`
    fn parse(&self, input: PState<'a, S>) -> PResult<'a, S, T>;

    /// A wrapper around `parse` for user-level parsing
    #[inline]
    fn run_parser(&self, file_name: &'a str, input: S) -> Result<T, PFailure<'a, S::Item>> {
        match self.parse(PState::new(file_name, input)) {
            Ok((x, _)) => Ok(x),
            Err(e) => Err(e),
        }
    }

    /// Directly runs `self` against `input` and pretty prints the error
    #[inline]
    fn test_parse(&self, input: S)
    where
        S::Item: Debug,
        T: Debug,
    {
        match self.run_parser("<TEST>", input) {
            Ok(x) => println!("{:#?}", x),
            Err(e) => println!("{:?}", e),
        }
    }

    /// Adds an `expected <name>` error message when the parser fails
    /// Intended to bring more readable parser errors to your parser
    #[inline]
    fn label<Str: ToString>(self, name: Str) -> impl Parser<'a, S, T>
    where
        Self: Sized,
    {
        move |input| match self.parse(input) {
            Ok((output, state)) => pok(output, state),

            Err(PFailure {
                location,
                consumption,
                unexpected,
                mut expected,
            }) => {
                expected.push(ErrorItem::Label(name.to_string()));
                perr(location, consumption, unexpected, expected)
            }
        }
    }

    #[inline]
    fn set_consumption(&self, consumption: Consumption) -> impl Parser<'a, S, T> {
        move |input| match self.parse(input) {
            Ok((output, state)) => pok(output, state.set_consumption(consumption)),
            Err(PFailure {
                location,
                unexpected,
                consumption: _,
                expected,
            }) => perr(location, consumption, unexpected, expected),
        }
    }

    #[inline]
    fn set_consuming(&self) -> impl Parser<'a, S, T> {
        move |input| match self.parse(input) {
            Ok((output, state)) => pok(output, state.set_consuming()),
            Err(PFailure {
                location,
                consumption: _,
                unexpected,
                expected,
            }) => perr(location, Consumption::Consuming, unexpected, expected),
        }
    }

    /// Maps over the result of `self`. This does not change the amount of input consumed
    #[inline]
    fn map<F, B>(&self, f: F) -> impl Parser<'a, S, B>
    where
        F: Fn(T) -> B,
    {
        move |input| match self.parse(input) {
            Ok((output, state)) => pok(f(output), state),
            Err(PFailure {
                location,
                consumption,
                unexpected,
                expected,
            }) => perr(location, consumption, unexpected, expected),
        }
    }

    /// Parses `self` without consuming any input
    #[inline]
    fn lookahead(&self) -> impl Parser<'a, S, T> {
        move |input| {
            let (x, _) = self.parse(input)?;
            pok(x, input.set_nonconsuming())
        }
    }

    /// Fails if the parser doesn't parse until EOF
    #[inline]
    fn exhaustive(&self) -> impl Parser<'a, S, T> {
        move |input| {
            let (x, s) = self.parse(input)?;
            let (_, s) = eof(s)?;
            pok(x, s)
        }
    }

    /// Creates a new parser based on previous input and makes it parse
    #[inline]
    fn bind<B, P, F>(&self, f: F) -> impl Parser<'a, S, B>
    where
        F: Fn(T) -> P,
        P: Parser<'a, S, B>,
    {
        move |input| {
            let (x, s) = self.parse(input)?;
            f(x).parse(s)
        }
    }

    #[inline]
    /// Replaces parser output
    fn replace<U: Clone>(&self, x: U) -> impl Parser<'a, S, U> {
        move |input| {
            let (_, s) = self.parse(input)?;
            pok(x.clone(), s)
        }
    }

    #[inline]
    /// Ignores parser output
    fn ignore(&self) -> impl Parser<'a, S, ()> {
        self.replace(())
    }
}

/// Creates a parser that doesn't consume input and always returns what was passed to it
#[inline]
pub fn pure<'a, S, T>(val: T) -> impl Parser<'a, S, T>
where
    S: Stream,
    T: Clone,
{
    move |input: PState<'a, S>| pok(val.clone(), input.set_nonconsuming())
}

/// Creates a parser that always fails with a chosen error message
#[inline]
pub fn fail<'a, Str, S, T>(message: Str) -> impl Parser<'a, S, T>
where
    S: Stream,
    Str: ToString,
{
    move |input: PState<'a, S>| {
        perr(
            input.location,
            Consumption::NonConsuming,
            None,
            vec![ErrorItem::Custom(message.to_string())],
        )
    }
}

#[inline]
/// A parser that always fails
pub fn empty<S, T>(input: PState<'_, S>) -> PResult<'_, S, T>
where
    S: Stream,
{
    perr(input.location, Consumption::NonConsuming, None, vec![])
}

/// Only parses succesfully when at the end of input.
#[inline]
pub fn eof<S>(state: PState<'_, S>) -> PResult<'_, S, ()>
where
    S: Stream,
{
    match state.input.uncons() {
        None => pok((), state.set_nonconsuming()),
        Some((x, _)) => perr(
            state.location,
            Consumption::NonConsuming,
            Some(ErrorItem::Tokens(vec![x])),
            vec![ErrorItem::EOF],
        ),
    }
}

#[inline]
pub fn satisfy_map<'a, S, F, P, T>(f: F, is_newline: P) -> impl Parser<'a, S, T>
where
    S: Stream,
    P: Fn(&S::Item) -> bool,
    F: Fn(&S::Item) -> Option<T>,
{
    move |input: PState<'a, S>| {
        let (x, s) = input.uncons(&is_newline)?;
        match f(&x) {
            Some(x) => pok(x, s),
            None => perr(
                s.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(x)),
                vec![],
            ),
        }
    }
}

#[inline]
pub fn satisfy_eq<'a, S>(x: S::Item, is_newline: bool) -> impl Parser<'a, S, S::Item>
where
    S: Stream,
    S::Item: PartialEq + Clone,
{
    move |input: PState<'a, S>| {
        let (y, s) = input.uncons(|_| is_newline)?;
        if x.clone() == y {
            pok(y, s)
        } else {
            perr(
                s.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(y)),
                vec![ErrorItem::Token(x.clone())],
            )
        }
    }
}

#[inline]
pub fn satisfy<'a, S, F, P>(f: F, is_newline: P) -> impl Parser<'a, S, S::Item>
where
    S: Stream,
    P: Fn(&S::Item) -> bool,
    F: Fn(&S::Item) -> bool,
{
    move |input: PState<'a, S>| {
        let (x, s) = input.uncons(&is_newline)?;
        if f(&x) {
            pok(x, s)
        } else {
            perr(
                input.location,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(x)),
                vec![],
            )
        }
    }
}

impl<'a, S, T, F> Parser<'a, S, T> for F
where
    F: Fn(PState<'a, S>) -> PResult<'a, S, T>,
    S: Stream,
{
    fn parse(&self, state: PState<'a, S>) -> PResult<'a, S, T> {
        self(state)
    }
}

impl<'a, S, T, P> Parser<'a, S, T> for Rc<P>
where
    P: Parser<'a, S, T>,
    S: Stream,
{
    fn parse(&self, state: PState<'a, S>) -> PResult<'a, S, T> {
        let x: &P = self.borrow();
        x.parse(state)
    }
}
