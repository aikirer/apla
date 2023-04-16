pub mod expr;

#[derive(Debug)]
pub enum AstNode {
    Expr(expr::Expr)
}

#[derive(Debug)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            nodes: vec![],
        }
    }
}

impl Default for Ast {
    fn default() -> Self {
        Self::new()
    }
}
