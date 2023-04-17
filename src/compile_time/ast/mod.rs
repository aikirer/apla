use crate::spanned::Spanned;

pub mod expr;

#[derive(Debug)]
pub enum AstNode {
    Expr(Spanned<expr::Expr>)
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
