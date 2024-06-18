// use crate::defs::Parser;
// use crate::defs::Stream;

use std::cmp::Ordering;

use crate::parser::perr;
use crate::parser::pok;
use crate::parser::Consumption;
use crate::parser::PFailure;
use crate::parser::PState;
use crate::parser::Parser;
use crate::stream::Stream;

pub trait Branching<'input, S, T>: Parser<'input, S, T>
where
    S: Stream,
{
    #[inline]
    /// The parser `p1.or(p2)` first parses `p1`, if it fails without consuming input, it uses `p2`
    /// as fallback
    fn or<P>(&self, fallback: P) -> impl Parser<'input, S, T>
    where
        P: Parser<'input, S, T>,
    {
        move |input| match self.parse(input) {
            Ok((output, state)) => pok(output, state),
            Err(PFailure {
                location,
                consumption: Consumption::Consuming,
                unexpected,
                expected,
            }) => perr(location, Consumption::Consuming, unexpected, expected),
            Err(PFailure {
                location: loc1,
                consumption: Consumption::NonConsuming,
                unexpected: unex1,
                expected: mut ex1,
            }) => match fallback.parse(input) {
                Ok((output, state)) => pok(output, state),
                Err(PFailure {
                    location: loc2,
                    consumption: cons2,
                    unexpected: unex2,
                    expected: mut ex2,
                }) => match loc1.cmp(&loc2) {
                    Ordering::Less => perr(loc2, cons2, unex2, ex2),
                    Ordering::Greater => perr(loc1, Consumption::NonConsuming, unex1, ex1),
                    Ordering::Equal => {
                        let unexpected = if let Some(e1) = unex1 {
                            if let Some(e2) = unex2 {
                                Some(e1.merge(e2))
                            } else {
                                Some(e1)
                            }
                        } else {
                            unex2
                        };

                        ex1.append(&mut ex2);
                        perr(loc2, cons2, unexpected, ex1)
                    }
                },
            },
        }
    }

    #[inline]
    /// The parser `p1.or(p2)` first parses `p1`, if it fails without consuming input, it uses `p2`
    /// as fallback
    fn either<P, U>(&self, fallback: P) -> impl Parser<'input, S, Result<T, U>>
    where
        P: Parser<'input, S, U>,
    {
        move |input| match self.parse(input) {
            Ok((output, state)) => pok(Ok(output), state),
            Err(PFailure {
                location,
                consumption: Consumption::Consuming,
                unexpected,
                expected,
            }) => perr(location, Consumption::Consuming, unexpected, expected),
            Err(PFailure {
                location: loc1,
                consumption: Consumption::NonConsuming,
                unexpected: unex1,
                expected: mut ex1,
            }) => match fallback.parse(input) {
                Ok( ( output, state )) => pok(Err(output), state),
                Err(PFailure {
                    location: loc2,
                    consumption: cons2,
                    unexpected: unex2,
                    expected: mut ex2,
                }) => match loc1.cmp(&loc2) {
                    Ordering::Less => perr(loc2, cons2, unex2, ex2),
                    Ordering::Greater => {
                        perr(loc1, Consumption::NonConsuming, unex1, ex1)
                    }
                    Ordering::Equal => {
                        let unexpected = if let Some(e1) = unex1 {
                            if let Some(e2) = unex2 {
                                Some(e1.merge(e2))
                            } else {
                                Some(e1)
                            }
                        } else {
                            unex2
                        };

                        ex1.append(&mut ex2);
                        perr(loc2, cons2, unexpected, ex1)
                    }
                },
            },
        }
    }

    #[inline]
    /// Sets `self`'s failure as non-consuming, allowing backtracking on failed parses that consume
    /// input when using `.try()`
    fn attempt(&self) -> impl Parser<'input, S, T> {
        move |input| match self.parse(input) {
            Err(PFailure {
                location,
                consumption: _,
                unexpected,
                expected,
            }) => perr(location, Consumption::NonConsuming, unexpected, expected),
            ok => ok,
        }
    }

    #[inline]
    /// Parser succeeds if `self` succeeds or errors without consuming input. The parser still
    /// fails if input is partially consumed
    fn optional(&self) -> impl Parser<'input, S, Option<T>> {
        move |input| match self.parse(input) {
            Ok( ( output, state )) => pok(Some(output), state),
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

/// folds an entire array of parsers, you would most likely need to wrap each item in an `Rc<_>` if
/// the parser isn't already Cloneable
pub fn choice<'input, Parsers, P, S, T>(parsers: Parsers) -> impl Parser<'input, S, T>
where
    S: Stream,
    Parsers: Clone + IntoIterator<Item = P>,
    P: Parser<'input, S, T>,
{
    move |input: PState<'input, S>| {
        let initial_error =
            perr(input.location, Consumption::NonConsuming, None, vec![]);
        parsers
            .clone()
            .into_iter()
            .fold(initial_error, |accum, x| match accum {
                Err(PFailure {
                    location: loc1,
                    consumption: Consumption::NonConsuming,
                    unexpected: unex1,
                    expected: mut ex1,
                }) => match x.parse(input) {
                    Err(PFailure {
                        location: loc2,
                        consumption: cons2,
                        unexpected: unex2,
                        expected: mut ex2,
                    }) => match loc1.cmp(&loc2) {
                        Ordering::Less => perr(loc2, cons2, unex2, ex2),
                        Ordering::Greater => {
                            perr(loc1, Consumption::NonConsuming, unex1, ex1)
                        }
                        Ordering::Equal => {
                            let unexpected = if let Some(e1) = unex1 {
                                if let Some(e2) = unex2 {
                                    Some(e1.merge(e2))
                                } else {
                                    Some(e1)
                                }
                            } else {
                                unex2
                            };

                            ex1.append(&mut ex2);
                            perr(loc2, cons2, unexpected, ex1)
                        }
                    },
                    x => x,
                },
                x => x,
            })
    }
}

impl<'a, S, T, P: ?Sized> Branching<'a, S, T> for P
where
    S: Stream,
    P: Parser<'a, S, T>,
{
}
