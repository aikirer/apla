use crate::{spanned::Spanned};

use super::{expr::Expr, AstNode};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    VarCreation {
        is_mut: bool,
        name: Spanned<String>,
        ty: Spanned<String>,
        value: Option<Spanned<Expr>>,
    },
    Assignment {
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    Block {
        nodes: Vec<AstNode>
    },
    If {
        condition: Spanned<Expr>,
        true_branch: Box<AstNode>,
        false_branch: Option<Box<AstNode>>,
    },
    Return {
        val: Option<Spanned<Expr>>,
    },
    While {
        condition: Spanned<Expr>,
        body: Box<AstNode>,
    },
    Break,
    Continue,

    Poison
}

impl Stmt {
    pub fn poison() -> Spanned<Self> {
        Spanned::new(
            Self::Poison, 0, 0
        )
    }
}
