use std::{error::Error, fmt::Display};

use crate::{token::Token, expr_type::ExprType, spanned::Spanned};

use super::ast::expr::Operator;

#[derive(Debug)]
pub enum CTErrorKind {
    Expected(Token),
    ExpectedButFound(Token, Token),
    ExpectedType,
    Unexpected(Token),
    MismatchedTypes(ExprType, ExprType),
    CantUseOpForTypes(Operator, ExprType),
    CantNegateType(ExprType),
    ExpectedOperator,
    HadError,

    Poisoned,
}

#[derive(Debug)]
pub struct CTError {
    pub kind: CTErrorKind
}

impl CTError {
    pub fn new(kind: CTErrorKind) -> Self {
        Self {
            kind
        }
    }
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
            Self::ExpectedType => 
                format!("Expected a type!"),
            Self::CantUseOpForTypes(op, t) => 
                format!("Cannot use the operator '{}' for '{t}'!", op.to_string()),
            Self::MismatchedTypes(t1, t2) => 
                format!("Mismatched types: '{t1}', '{t2}'!"),
            Self::CantNegateType(t) => 
                format!("Type '{t}' cannot be negated!"),
            Self::ExpectedOperator => 
                format!("Expected an operator here!"),
            Self::HadError => "Had an error!".to_string(), // tmp
            Self::Poisoned => "poisoned".to_string(),
        })
    }
}

pub fn report_error(error: &Spanned<CTError>, text: &str) {
    print!(" | [error] {}", **error);
    let mut at_char = error.start;
    let mut at_line = 1;
    let line = text.lines().find(|line| {
        if line.len() <= at_char {
            at_char -= line.len();
            at_line += 1;
            false
        } else {
            true
        }
    });
    println!(" [line {at_line}]");
    let line = match line {
        Some(l) => l,
        None => {
            eprintln!(" | couldn't find the error in code!");
            return;
        }
    };
    let prev_len = line.len();
    let line = line.trim_start();
    let at_char = at_char - (prev_len - (line.len() - 1)) + 1;
    print!(" | {line}\n | ");
    for _ in 0..at_char {
        print!(" ");
    }
    for _ in 0..error.len {
        print!("^");
    }
    println!()
}
