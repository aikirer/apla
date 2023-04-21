use crate::{expr_type::ExprType, compile_time::ast::AstNode};

use super::variable::Variable;

#[derive(Debug)]
pub struct Func {
    pub return_type: String,
    pub args: Vec<AstNode>,
    pub node: AstNode,
}

impl Func {
    pub fn new(
        return_type: String, args: Vec<AstNode>,
        node: AstNode,
    ) -> Self {
        Self {
            return_type, args, node
        }
    }
}

pub struct ParsedFunc {
    pub return_type: ExprType,
    pub args: Vec<Variable>,
    pub node: Vec<AstNode>,
}


impl ParsedFunc {
    pub fn new(
        return_type: ExprType, args: Vec<Variable>,
        node: Vec<AstNode>,
    ) -> Self {
        Self {
            return_type, args, node
        }
    }
}
