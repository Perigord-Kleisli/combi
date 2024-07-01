use crate::parser::Parser;
use crate::state::pok;
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
            let (x,input) = self.parse(input)?;
            let (y,input) = p.parse(input)?;
            pok(f(x,y),input)
        }
    }

    #[inline]
    /// Version of `seq` that takes a reference instead
    fn seq_ref<F, P, U, V>(&self, p: &P, f: F) -> impl Parser<'input, S, V>
    where
        F: Fn(T, U) -> V,
        P: Parser<'input, S, U>,
    {
        move |input| {
            let (x,input) = self.parse(input)?;
            let (y,input) = p.parse(input)?;
            pok(f(x,y),input)
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
            p.parse_end(input)
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
            p.parse_end(input)
        }
    }

    #[inline]
    fn seq_l<P, U>(&self, p: P) -> impl Parser<'input, S, T>
    where
        P: Parser<'input, S, U>,
    {
        move |input| {
            let (x, input) = self.parse(input)?;
            let (_, input) = p.parse(input)?;
            pok(x,input)
        }
    }
}
impl<'a, S, T, P> Sequential<'a, S, T> for P
where
    S: Stream,
    P: Parser<'a, S, T>,
{
}
