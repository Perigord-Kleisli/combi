use std::{collections::HashSet, hash::Hash};

pub struct Dedup_<I>
where
    I: Iterator,
{
    iter: I,
    set: HashSet<I::Item>,
}

impl<I> Iterator for Dedup_<I>
where
    I: Iterator,
    I::Item: Eq + Hash + Clone,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let mut out: I::Item = self.iter.next()?;

        while self.set.contains(&out) {
            out = self.iter.next()?;
        }

        self.set.insert(out.clone());
        Some(out)
    }
}

pub trait Dedup: Iterator {
    fn dedup(self) -> Dedup_<Self>
    where
        Self: Sized,
    {
        Dedup_ {
            iter: self,
            set: HashSet::new(),
        }
    }
}

impl<I: Iterator> Dedup for I {}

#[derive(Debug)]
pub enum Either<T, U> {
    Left(T),
    Right(U),
}

impl<T, U> Either<T, U> {
    pub fn from_left<F>(self, f: F) -> T
    where
        F: Fn(U) -> T,
    {
        match self {
            Either::Left(x) => x,
            Either::Right(x) => f(x),
        }
    }

    pub fn from_right<F>(self, f: F) -> U
    where
        F: Fn(T) -> U,
    {
        match self {
            Either::Left(x) => f(x),
            Either::Right(x) => x,
        }
    }

    pub fn either<F, G, V>(self, f: F, g: G) -> V
    where
        F: Fn(T) -> V,
        G: Fn(U) -> V,
    {
        match self {
            Either::Left(x) => f(x),
            Either::Right(x) => g(x),
        }
    }

    pub fn left(self) -> Option<T> {
        match self {
            Either::Left(x) => Some(x),
            Either::Right(_) => None,
        }
    }

    pub fn right(self) -> Option<U> {
        match self {
            Either::Left(_) => None,
            Either::Right(x) => Some(x),
        }
    }

    pub fn as_ref(&self) -> Either<&T,&U> {
        match self {
            Either::Left(l) => Either::Left(l),
            Either::Right(r) => Either::Right(r),
        }
    }
}
