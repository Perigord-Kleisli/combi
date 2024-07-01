use crate::{
    error::{ErrorItem, PFailure},
    parser::Parser,
    stream::Stream,
};
use std::path::Path;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy)]
pub struct PState<'input, St, S> {
    pub input: S,
    pub location: SourceLoc<'input>,
    pub consumption: Consumption,
    pub user_state: St,
}

#[derive(Debug, Clone, Copy)]
pub struct SecondStagePState<'input, St, S> {
    pub input: S,
    pub location: SourceLoc<'input>,
    pub consumption: Consumption,
    pub user_state: St,
}

pub type PSuccess<'input, St, S, T> = (T, PState<'input, St, S>);
pub type SecondStagePSuccess<'input, St, S, T> = (T, SecondStagePState<'input, St, S>);

pub type PResult<'input, St, S, T> =
    Result<PSuccess<'input, St, S, T>, PFailure<'input, <S as Stream>::Item>>;
pub type SecondStagePResult<'input, St, S, T> =
    Result<SecondStagePSuccess<'input, St, S, T>, PFailure<'input, <S as Stream>::Item>>;

pub trait ParserResult<'input, St, S, T>
where
    S: Stream,
{
    fn to_second_stage(self) -> SecondStagePResult<'input, St, S, T>;
    fn to_first_stage(self) -> PResult<'input, St, S, T>;
}

impl<'input, St, S, T> ParserResult<'input, St, S, T> for PResult<'input, St, S, T>
where
    S: Stream,
{
    #[inline]
    fn to_second_stage(self) -> SecondStagePResult<'input, St, S, T> {
        match self {
            Ok((x, s)) => Ok((x, s.to_second_stage())),
            Err(e) => Err(e),
        }
    }

    #[inline]
    fn to_first_stage(self) -> PResult<'input, St, S, T> {
        self
    }
}

impl<'input, St, S, T> ParserResult<'input, St, S, T> for SecondStagePResult<'input, St, S, T>
where
    S: Stream,
{
    #[inline]
    fn to_second_stage(self) -> SecondStagePResult<'input, St, S, T> {
        self
    }

    #[inline]
    fn to_first_stage(self) -> PResult<'input, St, S, T> {
        match self {
            Ok((x, s)) => Ok((x, s.to_first_stage())),
            Err(e) => Err(e),
        }
    }
}

pub trait ParserState<'input, St, S>
where
    S: Stream,
{
    fn parse<T, P>(self, p: &P) -> SecondStagePResult<'input, St, S, T>
    where
        P: Parser<'input, St, S, T> + ?Sized;

    fn to_first_stage(self) -> PState<'input, St, S>;
    fn to_second_stage(self) -> SecondStagePState<'input, St, S>;
}

impl<'input, St, S: Stream> ParserState<'input, St, S> for PState<'input, St, S> {
    fn parse<T, P>(self, p: &P) -> SecondStagePResult<'input, St, S, T>
    where
        P: Parser<'input, St, S, T> + ?Sized,
    {
        p.raw_parse(self).to_second_stage()
    }

    #[inline]
    fn to_first_stage(self) -> PState<'input, St, S> {
        self
    }

    #[inline]
    fn to_second_stage(self) -> SecondStagePState<'input, St, S> {
        SecondStagePState {
            input: self.input,
            location: self.location,
            consumption: self.consumption,
            user_state: self.user_state,
        }
    }
}

impl<'input, S: Stream, St> ParserState<'input, St, S> for SecondStagePState<'input, St, S> {
    fn parse<T, P>(self, p: &P) -> SecondStagePResult<'input, St, S, T>
    where
        P: Parser<'input, St, S, T> + ?Sized,
    {
        if self.consumption == Consumption::Consuming {
            p.set_consuming()
                .raw_parse(self.to_first_stage())
                .to_second_stage()
        } else {
            p.raw_parse(self.to_first_stage()).to_second_stage()
        }
    }

    #[inline]
    fn to_first_stage(self) -> PState<'input, St, S> {
        PState {
            input: self.input,
            location: self.location,
            consumption: self.consumption,
            user_state: self.user_state,
        }
    }

    #[inline]
    fn to_second_stage(self) -> SecondStagePState<'input, St, S> {
        self
    }
}

#[inline]
pub fn pok<'a, St, S: Stream, T>(
    output: T,
    state: impl ParserState<'a, St, S>,
) -> PResult<'a, St, S, T> {
    Ok((output, state.to_first_stage()))
}

#[inline]
pub fn perr<S: Stream, St, T>(
    location: SourceLoc<'_>,
    consumption: Consumption,
    unexpected: Option<ErrorItem<S::Item>>,
    expected: Vec<ErrorItem<S::Item>>,
) -> PResult<'_, St, S, T> {
    Err(PFailure {
        location,
        consumption,
        unexpected,
        expected,
    })
}

impl<'input, S: Stream, St> PState<'input, St, S> {
    pub fn new(file: &'input str, input: S, user_state: St) -> Self {
        PState {
            input,
            location: SourceLoc {
                col: 0,
                line: 1,
                file: std::path::Path::new(file),
            },
            consumption: Consumption::NonConsuming,
            user_state,
        }
    }

    pub fn uncons<P>(self, is_newline: P) -> SecondStagePResult<'input, St, S, S::Item>
    where
        P: Fn(&S::Item) -> bool,
    {
        match self.input.uncons() {
            None => perr(
                self.location.advance_col(),
                Consumption::NonConsuming,
                Some(ErrorItem::EOF),
                vec![],
            )
            .to_second_stage(),
            Some((x, xs)) => {
                if is_newline(&x) {
                    pok(
                        x,
                        PState {
                            input: xs,
                            location: self.location.advance_line(),
                            consumption: Consumption::Consuming,
                            user_state: self.user_state,
                        },
                    )
                    .to_second_stage()
                } else {
                    pok(
                        x,
                        PState {
                            input: xs,
                            location: self.location.advance_col(),
                            consumption: Consumption::Consuming,
                            user_state: self.user_state,
                        },
                    )
                    .to_second_stage()
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

    #[inline]
    pub fn set_consumption(self, consumption: Consumption) -> Self {
        PState {
            input: self.input,
            location: self.location,
            consumption,
            user_state: self.user_state,
        }
    }

    #[inline]
    pub fn set_consuming(self) -> Self {
        PState {
            input: self.input,
            location: self.location,
            consumption: Consumption::Consuming,
            user_state: self.user_state,
        }
    }

    #[inline]
    pub fn set_nonconsuming(self) -> Self {
        PState {
            input: self.input,
            location: self.location,
            consumption: Consumption::NonConsuming,
            user_state: self.user_state,
        }
    }
}

impl<'input, S: Stream, St> SecondStagePState<'input, St, S> {
    pub fn uncons<P>(self, is_newline: P) -> SecondStagePResult<'input, St, S, S::Item>
    where
        P: Fn(&S::Item) -> bool,
    {
        match self.input.uncons() {
            None => perr(
                self.location.advance_col(),
                Consumption::NonConsuming,
                Some(ErrorItem::EOF),
                vec![],
            )
            .to_second_stage(),
            Some((x, xs)) => {
                if is_newline(&x) {
                    pok(
                        x,
                        PState {
                            input: xs,
                            location: self.location.advance_line(),
                            consumption: Consumption::Consuming,
                            user_state: self.user_state,
                        },
                    )
                    .to_second_stage()
                } else {
                    pok(
                        x,
                        PState {
                            input: xs,
                            location: self.location.advance_col(),
                            consumption: Consumption::Consuming,
                            user_state: self.user_state,
                        },
                    )
                    .to_second_stage()
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

    #[inline]
    pub fn set_consumption(self, consumption: Consumption) -> Self {
        SecondStagePState {
            input: self.input,
            location: self.location,
            consumption,
            user_state: self.user_state,
        }
    }

    #[inline]
    pub fn set_consuming(self) -> Self {
        SecondStagePState {
            input: self.input,
            location: self.location,
            consumption: Consumption::Consuming,
            user_state: self.user_state,
        }
    }

    #[inline]
    pub fn set_nonconsuming(self) -> Self {
        SecondStagePState {
            input: self.input,
            location: self.location,
            consumption: Consumption::NonConsuming,
            user_state: self.user_state,
        }
    }
}
