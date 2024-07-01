use crate::state::{Consumption, SourceLoc};

#[derive(Debug, Clone)]
pub enum ErrorItem<Item> {
    Tokens(Vec<Item>),
    Token(Item),
    Branch(Vec<ErrorItem<Item>>),
    Label(String),
    Custom(String),
    EOF,
}

impl<Item> ErrorItem<Item> {
    pub fn merge(self, e2: Self) -> Self {
        if let ErrorItem::Branch(mut branches) = self {
            match e2 {
                ErrorItem::Branch(mut b) => branches.append(&mut b),
                e => branches.push(e),
            }
            ErrorItem::Branch(branches)
        } else if let ErrorItem::Branch(mut branches) = e2 {
            branches.push(self);
            ErrorItem::Branch(branches)
        } else {
            ErrorItem::Branch(vec![self, e2])
        }
    }
}

#[derive(Debug)]
pub struct PFailure<'input, Item> {
    pub location: SourceLoc<'input>,
    pub unexpected: Option<ErrorItem<Item>>,
    pub consumption: Consumption,
    pub expected: Vec<ErrorItem<Item>>,
}

// TODO: Error prettifier
