// use super::branching::Branching;
// use crate::combinators::sequential::Sequential;
// use crate::defs::Parser;
// use crate::defs::Stream;

use std::rc::Rc;

use crate::combinators::branching::Branching;
use crate::combinators::sequential::Sequential;
use crate::error::PFailure;
use crate::state::{perr, pok, Consumption, PState};
use crate::util::Either;
use crate::{parser::*, stream::Stream};

pub trait Repeating<'input, St, S, T>:
    Parser<'input, St, S, T> + Branching<'input, St, S, T>
where
    S: Stream,
    St: Clone,
{
    /// repeatedly parses `self` atleast once until failure
    fn some(&self) -> impl Parser<'input, St, S, Vec<T>> {
        move |input: PState<'input, St, S>| match self.parse(input.clone()) {
            Err(e) => Err(e),
            Ok((x, mut input)) => {
                let mut out = vec![x];
                while let (Some(v), new_input) = self.optional().parse(input.clone())? {
                    out.push(v);
                    input = new_input;
                }
                pok(out, input)
            }
        }
    }

    /// repeatedly parse `self` until it fails
    #[inline]
    fn many(&self) -> impl Parser<'input, St, S, Vec<T>> {
        move |input: PState<'input, St, S>| match self.some().parse_end(input.clone()) {
            Ok((output, state)) => pok(output, state),
            Err(PFailure {
                location,
                consumption: Consumption::Consuming,
                unexpected,
                expected,
            }) => perr(location, Consumption::Consuming, unexpected, expected),
            Err(PFailure {
                location: _,
                consumption: Consumption::NonConsuming,
                unexpected: _,
                expected: _,
            }) => pok(vec![], input.set_nonconsuming()),
        }
    }
    #[inline]
    /// repeatedly parses `self` separated by `sep`
    fn sep_by<P, U>(&self, sep: P) -> impl Parser<'input, St, S, Vec<T>>
    where
        P: Parser<'input, St, S, U>,
    {
        move |input: PState<'input, St, S>| match self.parse(input.clone()) {
            Err(PFailure {
                location,
                consumption: Consumption::Consuming,
                unexpected,
                expected,
            }) => perr(location, Consumption::Consuming, unexpected, expected),
            Err(PFailure {
                location: _,
                consumption: Consumption::NonConsuming,
                unexpected: _,
                expected: _,
                // }) => panic!(),
            }) => pok(vec![], input.set_nonconsuming()),
            Ok((output, state)) => {
                let mut x = vec![output];
                let (mut xs, state) = sep.seq_ref_r(self).many().parse(state)?;
                x.append(&mut xs);
                pok(x, state)
            }
        }
    }

    #[inline]
    /// repeatedly parses `self` one or more times separated by `sep`
    fn sep_by1<P, U>(&self, sep: P) -> impl Parser<'input, St, S, Vec<T>>
    where
        P: Parser<'input, St, S, U>,
    {
        move |input| match self.parse(input) {
            Err(PFailure {
                location,
                consumption,
                unexpected,
                expected,
            }) => perr(location, consumption, unexpected, expected),
            Ok((output, state)) => {
                let mut x = vec![output];
                let (mut xs, state) = sep.seq_ref_r(self).many().parse(state)?;
                x.append(&mut xs);
                pok(x, state)
            }
        }
    }

    /// repeatedly parses `self` one or more times until `till` is parsed`
    fn some_till<P, U>(&self, till: P) -> impl Parser<'input, St, S, Vec<T>>
    where
        P: Parser<'input, St, S, U>,
        St: Clone,
        Self: Sized,
    {
        let till = Rc::new(till);
        move |input: PState<'input, St, S>| match till.clone().parse(input.clone()) {
            Err(PFailure {
                location,
                consumption: Consumption::Consuming,
                unexpected,
                expected,
            }) => perr(location, Consumption::Consuming, unexpected, expected),
            Ok((_, state)) => pok(vec![], state),
            Err(PFailure {
                location: _,
                consumption: _,
                unexpected: _,
                expected: _,
            }) => match self.parse(input) {
                Err(PFailure {
                    location,
                    consumption,
                    unexpected,
                    expected,
                }) => perr(location, consumption, unexpected, expected),
                Ok((output, mut input)) => {
                    let mut xs = vec![output];

                    while let (Some(l), new_input) =
                        self.either(till.clone()).optional().parse(input.clone())?
                    {
                        input = new_input;
                        match l {
                            Either::Left(x) => xs.push(x),
                            Either::Right(_) => {
                                break;
                            }
                        }
                    }

                    pok(xs, input)
                }
            },
        }
    }

    /// repeatedly parses self until `till` succeeds
    #[inline]
    fn many_till<P, U>(self, till: P) -> impl Parser<'input, St, S, Vec<T>>
    where
        P: Parser<'input, St, S, U>,
        Self: Sized,
    {
        let till = Rc::new(till);
        let p = Rc::new(self);
        move |input: PState<'input, St, S>| match p
            .clone()
            .some_till(Rc::clone(&till))
            .parse(input.clone())
        {
            Ok((x, input)) => pok(x, input),
            Err(PFailure {
                location: _,
                unexpected: _,
                consumption: Consumption::NonConsuming,
                expected: _,
            }) => pok(vec![], input.set_nonconsuming()),
            Err(PFailure {
                location,
                unexpected,
                consumption: Consumption::Consuming,
                expected,
            }) => perr(location, Consumption::Consuming, unexpected, expected),
        }
    }

    /// parses `self` exactly `n` times
    fn replicate(&self, n: usize) -> impl Parser<'input, St, S, Vec<T>> {
        move |mut state: PState<'input, St, S>| {
            let mut output = Vec::new();
            let mut consumption = Consumption::NonConsuming;

            while output.len() < n {
                let (x, new_state) = if consumption.is_consuming() {
                    self.set_consuming().raw_parse(state)?
                } else {
                    self.raw_parse(state)?
                };

                output.push(x);
                state = new_state;
                consumption = consumption.merge(state.consumption);
            }

            pok(output, state)
        }
    }

    /// make `self` parse for a minimum of `min` times and a maximum of `max` times
    fn range(&self, min: usize, max: usize) -> impl Parser<'input, St, S, Vec<T>> {
        move |mut state: PState<'input, St, S>| {
            let mut output = Vec::new();

            let mut consumption = Consumption::NonConsuming;

            while output.len() < min {
                let (x, new_state) = if consumption.is_consuming() {
                    self.set_consuming().raw_parse(state)?
                } else {
                    self.raw_parse(state)?
                };

                output.push(x);
                state = new_state;
                consumption = consumption.merge(state.consumption);
            }

            while output.len() < max {
                match self.raw_parse(state.clone()) {
                    Ok((x, new_state)) => {
                        output.push(x);
                        state = new_state;
                        consumption = consumption.merge(state.consumption);
                    }
                    Err(PFailure {
                        location: _,
                        unexpected: _,
                        consumption: Consumption::NonConsuming,
                        expected: _,
                    }) => break,
                    Err(PFailure {
                        location,
                        unexpected,
                        consumption: Consumption::Consuming,
                        expected,
                    }) => return perr(location, Consumption::Consuming, unexpected, expected),
                }
            }
            pok(output, state)
        }
    }

    fn chainl1<P, F>(&self, op: P) -> impl Parser<'input, St, S, T>
    where
        P: Parser<'input, St, S, F>,
        F: Fn(T, T) -> T,
        Self: Sized,
    {
        move |input| {
            let (mut x, mut input) = self.parse(input)?;

            while let (Some(f), new_input) = op
                .seq_ref(self, |f: F, y| move |x| f(x, y))
                .optional()
                .parse(input.clone())?
            {
                input = new_input;
                x = f(x);
            }

            pok(x, input)
        }
    }
}

impl<'a, St, S, T, P> Repeating<'a, St, S, T> for P
where
    S: Stream,
    St: Clone,
    P: Parser<'a, St, S, T>,
{
}
