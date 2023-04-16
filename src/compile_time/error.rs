use std::{error::Error, fmt::Display};

use crate::token::Token;

#[derive(Debug)]
pub enum CTErrorKind {
    Expected(Token),
    ExpectedButFound(Token, Token),
    Unexpected(Token),
}

#[derive(Debug)]
pub struct CTError {
    pub kind: CTErrorKind
}

impl Error for CTError {}

impl Display for CTError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for CTErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Expected(t) => format!("Expected '{t}'!"),
            Self::ExpectedButFound(t, t2) => 
                format!("Expected '{t}', found '{t2}'!"),
            Self::Unexpected(t) => 
                format!("Unexpected '{t}'!"),
        })
    }
}