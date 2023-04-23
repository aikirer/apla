use crate::spanned::Spanned;

pub mod expr;
pub mod stmt;

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Expr(Spanned<expr::Expr>),
    Stmt(Spanned<stmt::Stmt>),
}

#[derive(Debug)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
    pub text: Option<String>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            nodes: vec![],
            text: None,
        }
    }
}

impl Default for Ast {
    fn default() -> Self {
        Self::new()
    }
}
