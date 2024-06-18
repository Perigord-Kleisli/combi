use crate::parser::{pok, Parser};
use crate::stream::Stream;

pub trait Sequential<'input, S, T>: Parser<'input, S, T>
where
    S: Stream,
{
    #[inline]
    /// Sequentially parses 2 parsers and applies a function over their result
    fn seq<F, P, U, V>(&self, p: P, f: F) -> impl Parser<'input, S, V>
    where
        F: Fn(T, U) -> V,
        P: Parser<'input, S, U>,
    {
        move |input| {
            let (x1, input) = self.parse(input)?;
            if input.has_consumed() {
                let (x2, input) = p.set_consuming().parse(input)?;
                pok(f(x1, x2), input)
            } else {
                let (x2, input) = p.parse(input)?;
                pok(f(x1, x2), input)
            }
        }
    }

    #[inline]
    /// a version of of `seq_r` that takes a reference of a parser, to ease certain type-checker issues
    fn seq_ref_r<P, U>(&self, p: &P) -> impl Parser<'input, S, U>
    where
        P: Parser<'input, S, U> + ?Sized,
    {
        move |input| {
            let (_, input) = self.parse(input)?;
            if input.has_consumed() {
                let (x2, input) = p.set_consuming().parse(input)?;
                pok(x2, input)
            } else {
                let (x2, input) = p.parse(input)?;
                pok(x2, input)
            }
        }
    }

    #[inline]
    /// ignores result from `self` and takes result from `p`
    fn seq_r<P, U>(&self, p: P) -> impl Parser<'input, S, U>
    where
        P: Parser<'input, S, U>,
    {
        move |input| {
            let (_, input) = self.parse(input)?;
            if input.has_consumed() {
                let (x2, input) = p.set_consuming().parse(input)?;
                pok(x2, input)
            } else {
                let (x2, input) = p.parse(input)?;
                pok(x2, input)
            }
        }
    }

    #[inline]
    fn seq_l<P, U>(&self, p: P) -> impl Parser<'input, S, T>
    where
        P: Parser<'input, S, U>,
    {
        move |input| {
            let (x1, input) = self.parse(input)?;
            if input.has_consumed() {
                let (_, input) = p.set_consuming().parse(input)?;
                pok(x1, input)
            } else {
                let (_, input) = p.parse(input)?;
                pok(x1, input)
            }
        }
    }
}
impl<'a, S, T, P> Sequential<'a, S, T> for P
where
    S: Stream,
    P: Parser<'a, S, T>,
{
}
