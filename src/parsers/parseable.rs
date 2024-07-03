use core::f32;
use std::u128;

use crate::{
    choice,
    combinators::{branching::Branching, repeating::Repeating, sequential::Sequential},
    parser::Parser,
    parsers::char::{char, class::digit, string},
    state::{pok, PResult, PState},
    stream::Stream,
};

pub trait Parseable<St, S: Stream>: Sized {
    fn parser(input: PState<'_, St, S>) -> PResult<'_, St, S, Self>;
}

impl<St, S> Parseable<St, S> for bool
where
    St: Clone,
    S: Stream<Item = char>,
{
    fn parser(input: PState<'_, St, S>) -> PResult<'_, St, S, Self> {
        choice![string("true").replace(true), string("false").replace(false)]
            .label("bool")
            .parse_end(input)
    }
}

macro_rules! unsigned_parser {
    ($x:ty,$name:expr) => {
        impl<St, S> Parseable<St, S> for $x
        where
            St: Clone,
            S: Stream<Item = char>,
        {
            fn parser(input: PState<'_, St, S>) -> PResult<'_, St, S, Self> {
                let (s, input) = digit.some().label($name).parse(input)?;
                let num = s
                    .into_iter()
                    .fold(0, |n, c| n * 10 + (c.to_digit(10).unwrap() as $x));
                pok(num, input)
            }
        }
    };
}

unsigned_parser!(usize, "usize");
unsigned_parser!(u128, "u32");
unsigned_parser!(u64, "u32");
unsigned_parser!(u32, "u32");
unsigned_parser!(u16, "u16");
unsigned_parser!(u8, "u8");

macro_rules! signed_parser {
    ($t:ty,$name:expr) => {
        impl<St, S> Parseable<St, S> for $t
        where
            St: Clone,
            S: Stream<Item = char>,
        {
            fn parser(input: PState<'_, St, S>) -> PResult<'_, St, S, Self> {
                let (negative, input) = char('-').optional().map(|x| x.is_some()).parse(input)?;
                let (s, input) = digit.some().label($name).parse(input)?;
                let num = s
                    .into_iter()
                    .fold(0, |n, c| n * 10 + (c.to_digit(10).unwrap() as $t));
                pok(if negative { -num } else { num }, input)
            }
        }
    };
}

signed_parser!(i8, "i8");
signed_parser!(i16, "i16");
signed_parser!(i32, "i32");
signed_parser!(i64, "i64");
signed_parser!(i128, "i128");

macro_rules! float_parser {
    ($t:ty,$name:expr) => {
        impl<St, S> Parseable<St, S> for $t
        where
            St: Clone,
            S: Stream<Item = char>,
        {
            fn parser(input: PState<'_, St, S>) -> PResult<'_, St, S, Self> {
                let (negative, input) = char('-').optional().map(|x| x.is_some()).parse(input)?;
                let (characteristic, input) = digit.some().label($name).parse(input)?;
                let characteristic = characteristic
                    .into_iter()
                    .fold(0.0, |n, c| n * 10.0 + (c.to_digit(10).unwrap() as $t));
                let (mantissa, input) = char('.')
                    .seq_r(digit.some())
                    .label($name)
                    .optional()
                    .parse(input)?;
                let val = if let Some(mantissa) = mantissa {
                    characteristic
                        + mantissa
                            .into_iter()
                            .rev()
                            .fold(0.0, |n, c| n / 10.0 + (c.to_digit(10).unwrap() as $t))
                            / 10.0
                } else {
                    characteristic
                };
                pok(if negative { -val } else { val }, input)
            }
        }
    };
}

float_parser!(f32,"f32");
float_parser!(f64,"f64");
