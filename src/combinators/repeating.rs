// use super::branching::Branching;
// use crate::combinators::sequential::Sequential;
// use crate::defs::Parser;
// use crate::defs::Stream;

use std::rc::Rc;

use crate::combinators::branching::Branching;
use crate::combinators::sequential::Sequential;
use crate::{parser::*, stream::Stream};

pub trait Repeating<'input, S, T>: Parser<'input, S, T>
where
    S: Stream,
{
    /// repeatedly parses `self` atleast once until failure
    fn some(&self) -> impl Parser<'input, S, Vec<T>> {
        move |input| match self.parse(input) {
            Err(PFailure {
                location,
                consumption,
                unexpected,
                expected,
            }) => perr(location, consumption, unexpected, expected),
            Ok((output, mut state)) => {
                let mut consumption = state.consumption;
                let mut xs = vec![output];

                loop {
                    match self.parse(state) {
                        Ok((output, new_state)) => {
                            xs.push(output);
                            state = new_state;
                            consumption = consumption.merge(state.consumption);
                        }
                        Err(PFailure {
                            location,
                            consumption,
                            unexpected,
                            expected,
                        }) => match consumption {
                            Consumption::Consuming => {
                                return perr(location, consumption, unexpected, expected)
                            }
                            Consumption::NonConsuming => break,
                        },
                    }
                }
                pok(xs, state.set_consumption(consumption))
            }
        }
    }

    /// repeatedly parse `self` until it fails
    #[inline]
    fn many(&self) -> impl Parser<'input, S, Vec<T>> {
        move |input| match self.some().parse(input) {
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
    fn sep_by<P, U>(&self, sep: P) -> impl Parser<'input, S, Vec<T>>
    where
        P: Parser<'input, S, U>,
    {
        move |input| match self.parse(input) {
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
    fn sep_by1<P, U>(&self, sep: P) -> impl Parser<'input, S, Vec<T>>
    where
        P: Parser<'input, S, U>,
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
    fn some_till<P, U>(&self, till: P) -> impl Parser<'input, S, Vec<T>>
    where
        P: Parser<'input, S, U>,
    {
        let till = Rc::new(till);
        move |input| match till.clone().parse(input) {
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
                Ok((output, mut state)) => {
                    let mut xs = vec![output];
                    let mut consumption = state.consumption;

                    loop {
                        match self.either(till.clone()).parse(state) {
                            Ok((Ok(x), new_state)) => {
                                xs.push(x);
                                state = new_state;
                                consumption = consumption.merge(state.consumption);
                            }
                            Ok((Err(_), new_state)) => {
                                state = new_state;
                                consumption = consumption.merge(state.consumption);
                                break;
                            }
                            Err(PFailure {
                                location,
                                consumption,
                                unexpected,
                                expected,
                            }) => return perr(location, consumption, unexpected, expected),
                        }
                    }
                    pok(xs, state.set_consumption(consumption))
                }
            },
        }
    }

    /// repeatedly parses self until `till` succeeds
    fn many_till<P, U>(&self, till: P) -> impl Parser<'input, S, Vec<T>>
    where
        P: Parser<'input, S, U>,
    {
        let till = Rc::new(till);
        move |input| match self.some_till(Rc::clone(&till)).parse(input) {
            Ok(x) => Ok(x),
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
    fn replicate(&self, n: usize) -> impl Parser<'input, S, Vec<T>> {
        move |mut state: PState<'input, S>| {
            let mut output = Vec::new();
            let mut consumption = Consumption::NonConsuming;

            while output.len() < n {
                let (x, new_state) = if consumption.is_consuming() {
                    self.set_consuming().parse(state)?
                } else {
                    self.parse(state)?
                };

                output.push(x);
                state = new_state;
                consumption = consumption.merge(state.consumption);
            }

            pok(output, state)
        }
    }

    /// make `self` parse for a minimum of `min` times and a maximum of `max` times
    fn range(&self, min: usize, max: usize) -> impl Parser<'input, S, Vec<T>> {
        move |mut state: PState<'input, S>| {
            let mut output = Vec::new();

            let mut consumption = Consumption::NonConsuming;

            while output.len() < min {
                let (x, new_state) = if consumption.is_consuming() {
                    self.set_consuming().parse(state)?
                } else {
                    self.parse(state)?
                };

                output.push(x);
                state = new_state;
                consumption = consumption.merge(state.consumption);
            }

            while output.len() < max {
                match self.parse(state) {
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
}

impl<'a, S, T, P> Repeating<'a, S, T> for P
where
    S: Stream + Copy,
    P: Parser<'a, S, T>,
{
}
