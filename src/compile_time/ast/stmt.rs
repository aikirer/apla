use crate::{spanned::Spanned};

use super::{expr::Expr, AstNode};

#[derive(Debug)]
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

    Poison
}
