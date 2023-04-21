use std::{error::Error, fmt::Display};

use crate::expr_type::ExprType;

#[derive(Debug)]
pub enum RTError {
    WrongType(ExprType), EmptyStack,
    MismatchedTypes(ExprType, ExprType),
    VarDoesntExist(String),
    ExpectedPlace,
    ExpectedBool,
}

impl Error for RTError {}

impl Display for RTError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t = match self {
            RTError::WrongType(t) => 
                format!("'{t}' is not a valid type here!"),
            RTError::MismatchedTypes(expected, found) => 
                format!("expected '{expected}', found '{found}'!"),
            Self::EmptyStack => "The stack was empty!".to_string(),
            Self::VarDoesntExist(n) => 
                format!("Variable {n} doesn't exist!"),
            Self::ExpectedPlace => 
                format!("Expected a place expression!"),
            Self::ExpectedBool => 
                format!("Expected a boolean value!"),
        };
        write!(f, "{t}")
    }
}