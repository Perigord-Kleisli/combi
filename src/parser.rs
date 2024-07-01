use std::borrow::Borrow;
use std::fmt::Debug;
use std::rc::Rc;

pub use crate::combinators::branching;
pub use crate::combinators::repeating;
pub use crate::combinators::sequential;
pub use crate::error::*;
pub use crate::state::*;
pub use crate::stream::Stream;

// A general trait for parsers

pub trait Parser<'input, St, S, T>
where
    S: Stream,
{
    // /// A direct implementation intended only for parser implementations
    // /// If intended to be ran by other parsers use `.parse()`
    // /// If intended to be ran directly by the user, instead use `run_parse`
    fn raw_parse(&self, input: PState<'input, St, S>) -> PResult<'input, St, S, T>;

    #[inline]
    fn parse(
        &self,
        input: impl ParserState<'input, St, S>,
    ) -> SecondStagePResult<'input, St, S, T> {
        input.parse(self)
    }

    #[inline]
    fn parse_end(&self, input: impl ParserState<'input, St, S>) -> PResult<'input, St, S, T> {
        input.parse(self).to_first_stage()
    }

    /// A wrapper around `parse` for user-level parsing
    #[inline]
    fn run_parser(
        &self,
        file_name: &'input str,
        user_state: St,
        input: S,
    ) -> Result<T, PFailure<'input, S::Item>> {
        match self.raw_parse(PState::new(file_name, input, user_state)) {
            Ok((x, _)) => Ok(x),
            Err(e) => Err(e),
        }
    }

    /// Directly runs `self` against `input` and pretty prints the error
    #[inline]
    fn test_parse(&self, user_state: St, input: S)
    where
        S::Item: Debug,
        T: Debug,
    {
        match self.run_parser("<TEST>", user_state, input) {
            Ok(x) => println!("{:#?}", x),
            Err(e) => println!("{:?}", e),
        }
    }

    #[inline]
    fn map_err<F>(&self, f: F) -> impl Parser<'input, St, S, T>
    where
        F: Fn(PFailure<'input, S::Item>) -> PFailure<'input, S::Item>,
    {
        move |input| match self.parse(input) {
            Ok((x, s)) => pok(x, s),
            Err(e) => Err(f(e)),
        }
    }

    #[inline]
    fn bind_err<F>(&self, f: F) -> impl Parser<'input, St, S, T>
    where
        F: Fn(PFailure<'input, S::Item>) -> PResult<'input, St, S, T>,
    {
        move |input| match self.parse(input) {
            Ok((x, s)) => pok(x, s),
            Err(e) => f(e),
        }
    }

    #[inline]
    /// Adds an `expected <name>` error message when the parser fails
    /// Intended to bring more readable parser errors to your parser
    fn label<Str: ToString>(&self, name: Str) -> impl Parser<'input, St, S, T> {
        self.map_err(
            move |PFailure {
                      location,
                      unexpected,
                      consumption,
                      mut expected,
                  }| {
                expected.push(ErrorItem::Label(name.to_string()));
                PFailure {
                    location,
                    consumption,
                    unexpected,
                    expected,
                }
            },
        )
    }

    #[inline]
    fn set_consumption(&self, consumption: Consumption) -> impl Parser<'input, St, S, T> {
        move |input| match self.raw_parse(input) {
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
    fn set_consuming(&self) -> impl Parser<'input, St, S, T> {
        self.set_consumption(Consumption::Consuming)
    }

    #[inline]
    fn set_nonconsuming(&self) -> impl Parser<'input, St, S, T> {
        self.set_consumption(Consumption::NonConsuming)
    }

    /// Maps over the result of `self`. This does not change the amount of input consumed
    #[inline]
    fn map<F, B>(&self, f: F) -> impl Parser<'input, St, S, B>
    where
        F: Fn(T) -> B,
    {
        move |input| match self.raw_parse(input) {
            Ok((output, state)) => pok(f(output), state),
            Err(e) => Err(e),
        }
    }

    /// Parses `self` without consuming any input
    #[inline]
    fn lookahead(&self) -> impl Parser<'input, St, S, T>
    where
        St: Clone,
    {
        move |s: PState<'input, St, S>| {
            let (x, _) = self.parse(s.clone())?;
            pok(x, s.set_nonconsuming())
        }
    }

    /// Fails if the parser doesn't parse until EOF
    #[inline]
    fn exhaustive(&self) -> impl Parser<'input, St, S, T> {
        move |input| {
            let (x, s) = self.parse(input)?;
            let (_, s) = eof.parse(s)?;
            pok(x, s)
        }
    }

    /// Creates a new parser based on previous input and makes it parse
    #[inline]
    fn bind<B, P, F>(&self, f: F) -> impl Parser<'input, St, S, B>
    where
        F: Fn(T) -> P,
        P: Parser<'input, St, S, B>,
    {
        move |input| {
            let (x, s) = self.raw_parse(input)?;
            if s.has_consumed() {
                f(x).set_consuming().raw_parse(s)
            } else {
                f(x).raw_parse(s)
            }
        }
    }

    #[inline]
    /// Replaces parser output
    fn replace<U: Clone>(&self, x: U) -> impl Parser<'input, St, S, U> {
        move |input| {
            let (_, s) = self.parse(input)?;
            pok(x.clone(), s)
        }
    }

    #[inline]
    /// Ignores parser output
    fn ignore(&self) -> impl Parser<'input, St, S, ()> {
        self.replace(())
    }

    #[inline]
    fn modify_state<F>(&self, f: F) -> impl Parser<'input, St, S, ()>
    where
        F: Fn(St) -> St,
    {
        move |mut input: PState<'input, St, S>| {
            input.user_state = f(input.user_state);
            pok((), input)
        }
    }

    #[inline]
    fn put_state(&self, state: St) -> impl Parser<'input, St, S, ()>
    where
        St: Clone,
    {
        move |mut input: PState<'input, St, S>| {
            input.user_state = state.clone();
            pok((), input)
        }
    }

    fn get_state(&self) -> impl Parser<'input, St, S, St>
    where
        St: Clone,
    {
        |input: PState<'input, St, S>| pok(input.user_state.clone(), input)
    }
}

impl<'a, St, S, T, F> Parser<'a, St, S, T> for F
where
    F: Fn(PState<'a, St, S>) -> PResult<'a, St, S, T>,
    S: Stream,
{
    fn raw_parse(&self, state: PState<'a, St, S>) -> PResult<'a, St, S, T> {
        self(state)
    }
}

impl<'a, St, S, T, P> Parser<'a, St, S, T> for Rc<P>
where
    P: Parser<'a, St, S, T>,
    S: Stream,
{
    fn raw_parse(&self, state: PState<'a, St, S>) -> PResult<'a, St, S, T> {
        let x: &P = self.borrow();
        x.raw_parse(state)
    }
}

#[inline]
/// Creates a parser that doesn't consume input and always returns what was passed to it
pub fn pure<'a, St, S, T>(val: T) -> impl Parser<'a, St, S, T>
where
    S: Stream,
    T: Clone,
{
    move |input: PState<'a, St, S>| pok(val.clone(), input.set_nonconsuming())
}

#[inline]
/// Creates a parser that always fails with a chosen error message
pub fn fail<'a, Str, St, S, T>(message: Str) -> impl Parser<'a, St, S, T>
where
    S: Stream,
    Str: ToString,
{
    move |input: PState<'a, St, S>| {
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
pub fn empty<St, S, T>(input: PState<'_, St, S>) -> PResult<'_, St, S, T>
where
    S: Stream,
{
    perr(input.location, Consumption::NonConsuming, None, vec![])
}

/// Only parses succesfully when at the end of input.
#[inline]
pub fn eof<St, S>(state: PState<'_, St, S>) -> PResult<'_, St, S, ()>
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
pub fn satisfy_map<'a, St, S, F, P, T>(f: F, is_newline: P) -> impl Parser<'a, St, S, T>
where
    S: Stream,
    P: Fn(&S::Item) -> bool,
    F: Fn(&S::Item) -> Option<T>,
{
    move |input: PState<'a, St, S>| {
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
pub fn satisfy_eq<'a, St, S>(x: S::Item, is_newline: bool) -> impl Parser<'a, St, S, S::Item>
where
    S: Stream,
    S::Item: PartialEq + Clone,
{
    move |input: PState<'a, St, S>| {
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
pub fn satisfy<'a, St, S, F, P>(f: F, is_newline: P) -> impl Parser<'a, St, S, S::Item>
where
    S: Stream,
    P: Fn(&S::Item) -> bool,
    F: Fn(&S::Item) -> bool,
{
    move |input: PState<'a, St, S>| {
        let loc = input.location;
        let (x, s) = input.uncons(&is_newline)?;
        if f(&x) {
            pok(x, s)
        } else {
            perr(
                loc,
                Consumption::NonConsuming,
                Some(ErrorItem::Token(x)),
                vec![],
            )
        }
    }
}
