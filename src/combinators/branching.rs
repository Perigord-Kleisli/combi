// use crate::defs::Parser;
// use crate::defs::Stream;

use std::cmp::Ordering;

use crate::error::{ErrorItem, PFailure};
use crate::parser::Parser;
use crate::state::{perr, pok, Consumption, PState};
use crate::stream::Stream;
use crate::util::Either;

fn merge_unexpected<Item>(
    unexpected_1: Option<ErrorItem<Item>>,
    unexpected_2: Option<ErrorItem<Item>>,
) -> Option<ErrorItem<Item>> {
    match (unexpected_1, unexpected_2) {
        (None, None) => None,
        (Some(x), None) => Some(x),
        (None, Some(x)) => Some(x),
        (Some(ErrorItem::Branch(mut x)), Some(ErrorItem::Branch(mut y))) => {
            x.append(&mut y);
            Some(ErrorItem::Branch(x))
        }
        (Some(ErrorItem::Branch(mut x)), Some(y)) => {
            x.push(y);
            Some(ErrorItem::Branch(x))
        }
        (Some(x), Some(ErrorItem::Branch(mut y))) => {
            y.push(x);
            Some(ErrorItem::Branch(y))
        }
        (Some(x), Some(y)) => Some(ErrorItem::Branch(vec![x, y])),
    }
}

fn merge_pfailures<'a, Item>(
    mut e1: PFailure<'a, Item>,
    mut e2: PFailure<'a, Item>,
) -> PFailure<'a, Item> {
    match e1.location.cmp(&e2.location) {
        Ordering::Less => e2,
        Ordering::Greater => e1,
        Ordering::Equal => {
            e1.expected.append(&mut e2.expected);
            PFailure {
                location: e1.location,
                unexpected: merge_unexpected(e1.unexpected, e2.unexpected),
                consumption: e1.consumption.merge(e2.consumption),
                expected: e1.expected,
            }
        }
    }
}

pub trait Branching<'input, St, S, T>: Parser<'input, St, S, T>
where
    S: Stream,
    St: Clone
{
    #[inline]
    /// The parser `p1.or(p2)` first parses `p1`, if it fails without consuming input, it uses `p2`
    /// as fallback
    fn or<P>(&self, fallback: P) -> impl Parser<'input, St, S, T>
    where
        P: Parser<'input, St, S, T>,
    {
        move |input: PState<'input, St, S>| match self.parse(input.clone()) {
            Ok((x, input)) => pok(x, input),
            Err(e) if e.consumption == Consumption::Consuming => Err(e),
            Err(e1) => match fallback.parse(input) {
                Ok((x, input)) => pok(x, input),
                Err(e2) => Err(merge_pfailures(e1, e2)),
            },
        }
    }

    #[inline]
    /// The parser `p1.or(p2)` first parses `p1`, if it fails without consuming input, it uses `p2`
    /// as fallback
    fn or_ref<P>(&self, fallback: &P) -> impl Parser<'input, St, S, T>
    where
        P: Parser<'input, St, S, T>,
    {
        move |input: PState<'input, St, S>| match self.parse(input.clone()) {
            Ok((x, input)) => pok(x, input),
            Err(e) if e.consumption == Consumption::Consuming => Err(e),
            Err(e1) => match fallback.parse(input) {
                Ok((x, input)) => pok(x, input),
                Err(e2) => Err(merge_pfailures(e1, e2)),
            },
        }
    }

    #[inline]
    /// The parser `p1.or(p2)` first parses `p1`, if it fails without consuming input, it uses `p2`
    /// as fallback
    fn either<P, U>(&self, fallback: P) -> impl Parser<'input, St, S, Either<T, U>>
    where
        P: Parser<'input, St, S, U>,
    {
        move |input: PState<'input, St, S>| match self.parse(input.clone()) {
            Ok((x, input)) => pok(Either::Left(x), input),
            Err(e) if e.consumption == Consumption::Consuming => Err(e),
            Err(e1) => match fallback.parse(input) {
                Ok((x, input)) => pok(Either::Right(x), input),
                Err(e2) => Err(merge_pfailures(e1, e2)),
            },
        }
    }

    #[inline]
    /// Creates a parser that returns `val` when `self` fails without consuming input
    fn or_pure(&self, val: T) -> impl Parser<'input, St, S, T>
    where
        T: Clone,
    {
        move |input: PState<'input, St, S>| match self.parse(input.clone()) {
            Ok((x, input)) => pok(x, input),
            Err(PFailure {
                location,
                unexpected,
                consumption: Consumption::Consuming,
                expected,
            }) => perr(location, Consumption::Consuming, unexpected, expected),
            Err(PFailure {
                location: _,
                unexpected: _,
                consumption: Consumption::NonConsuming,
                expected: _,
            }) => pok(val.clone(), input.set_nonconsuming()),
        }
    }

    #[inline]
    /// Sets `self`'s failure as non-consuming, allowing backtracking on failed parses that consume
    /// input when using `.attempt()`
    fn attempt(&self) -> impl Parser<'input, St, S, T> {
        move |input| match self.parse(input) {
            Err(PFailure {
                location,
                consumption: _,
                unexpected,
                expected,
            }) => perr(location, Consumption::NonConsuming, unexpected, expected),
            Ok((x, s)) => pok(x, s),
        }
    }

    #[inline]
    /// Parser succeeds if `self` succeeds or errors without consuming input. The parser still
    /// fails if input is partially consumed
    fn optional(&self) -> impl Parser<'input, St, S, Option<T>>
    {
        move |input: PState<'input, St, S>| match self.parse(input.clone()) {
            Ok((output, state)) => pok(Some(output), state),
            Err(PFailure {
                location: _,
                consumption: Consumption::NonConsuming,
                unexpected: _,
                expected: _,
            }) => pok(None, input.set_nonconsuming()),
            Err(PFailure {
                location,
                consumption,
                unexpected,
                expected,
            }) => perr(location, consumption, unexpected, expected),
        }
    }
}

#[macro_export]
/// `choice![p1,p2,p3]` == `p1.or(p2.or(p3))`
macro_rules! choice {
    [$x:expr] => ($x);
    [$x:expr, $($xs:expr),+] => {$x.or(choice!($($xs),+))};
}

impl<'a, St, S, T, P> Branching<'a, St, S, T> for P
where
    S: Stream,
    St: Clone,
    P: Parser<'a, St, S, T>,
{
}
