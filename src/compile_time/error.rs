use std::{error::Error, fmt::Display};

use crate::{token::Token, expr_type::ExprType, spanned::Spanned};

use super::ast::expr::Operator;

#[derive(Debug, PartialEq, Clone)]
pub enum CTErrorKind {
    Expected(Token),
    ExpectedButFound(Token, Token),
    ExpectedType,
    Unexpected(Token),
    MismatchedTypes(ExprType, ExprType),
    CantUseOpForTypes(Operator, ExprType),
    CantNegateType(ExprType),
    VarDoesntExist(String),
    UninitVarUsed(String),
    CantIndexType(ExprType),
    CantUseGet,
    ExpectedOperator,
    ExpectedStmt,
    ExpectedExpr,
    HadError,
    ExpectedIdent,
    ExpectedStr,
    CantInferType,
    ExpectedPlace,
    ExpectedVarCreation,
    CantAssignToConst,
    CantAssignToThis,
    TypeNotAnnotated,
    ExpectedFuncCall,
    CantIndex,
    CantDeref,

    WrongArgCount(usize),
    FuncDoesntExist(String),

    Poisoned,
}

#[derive(Debug, Clone, PartialEq)]
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
            Self::ExpectedType => "Expected a type!".to_string(),
            Self::CantUseOpForTypes(op, t) => 
                format!("Cannot use the operator '{op}' for '{t}'!"),
            Self::MismatchedTypes(t1, t2) => 
                format!("Mismatched types: '{t1}', '{t2}'!"),
            Self::CantNegateType(t) => 
                format!("Type '{t}' cannot be negated!"),
            Self::ExpectedOperator => 
                "Expected an operator here!".to_string(),
            Self::VarDoesntExist(n) => 
                format!("Var '{n}' doesn't exist!"),
            Self::UninitVarUsed(n) => 
                format!("var '{n}' cannot be used when uninitialized!"),
            Self::CantIndexType(t) => 
                format!("Type '{t}' cannot be indexed!"),
            Self::CantUseGet => "Cannot use the '.' operator here!".to_string(),
            Self::HadError => "Had an error!".to_string(), // tmp
            Self::ExpectedExpr => "Expected an expression!".to_string(),
            Self::ExpectedStmt => "Expected a statement!".to_string(),
            Self::ExpectedPlace => "Expected a place expression!".to_string(),
            Self::ExpectedIdent => "Expected an identifier!".to_string(),
            Self::ExpectedStr => "Expected a string!".to_string(),
            Self::ExpectedVarCreation => "Expected a variable declaration!".to_string(),
            Self::CantInferType => "Cannot infer the type!".to_string(),
            Self::CantAssignToConst => 
                "Cannot assign to a constant variable!".to_string(),
            Self::CantAssignToThis => 
                "Cannot assign to this!".to_string(),
            Self::TypeNotAnnotated =>
                "The type needs to be annotated explicitly for this variable!".to_string(),
            Self::WrongArgCount(count) =>
                format!("This function takes {count} arguments!"),
            Self::ExpectedFuncCall =>
                "Expected a function call!".to_string(),
            Self::FuncDoesntExist(name) =>
                format!("Function '{name}' doesn't exist!"),
            Self::CantDeref =>
                format!("This cannot be dereferenced!"),
            Self::CantIndex => "Indexing is unstable!".to_string(),
            Self::Poisoned => "poisoned".to_string(),
        })
    }
}

pub fn report_error(error: &Spanned<CTError>, text: &str) {
    print!(" | [error] {}", **error);
    // that doesn't look very good
    let mut at_char = error.start;
    let mut at_char_on_new_line = at_char;
    if at_char >= text.len() {
        println!("\n | Couldn't find the token in the code! (Does the error occur at the end of the file?)");
        return;
    }
    let at_line = text[..at_char].chars().filter(|c| {
        at_char -= 1;
        if *c == '\n' {
            at_char_on_new_line = at_char;
            true
        } else { false }
    }).count();
    let at_char = at_char_on_new_line;
    let line = text.lines().nth(at_line).unwrap();
    println!(" [line {}]", at_line + 1);
    let prev_len = line.len() - 1;
    let line = line.trim_start();
    let at_char = at_char - (prev_len - (line.len() - 1));
    print!(" | {line}\n | ");
    for _ in 0..at_char {
        print!(" ");
    }
    for _ in 0..error.len {
        print!("^");
    }
    println!()
}
