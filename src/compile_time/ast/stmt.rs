use crate::{spanned::Spanned};

use super::expr::Expr;

#[derive(Debug)]
pub enum Stmt {
    VarCreation {
        is_mut: bool,
        name: Spanned<String>,
        ty: Spanned<String>,
        value: Spanned<Expr>,
    }
}
