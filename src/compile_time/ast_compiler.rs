use crate::run_time::bytecode::OpCode;
use super::ast::{Ast, expr::Expr, AstNode};

use super::compile::{self, Compile};

pub struct Compiler<'a> {
    pub ast: &'a Ast,
}

impl<'a> Compiler<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self { ast }
    }

    pub fn compile(&mut self) -> compile::Output {
        self.ast.compile()
    }
}

impl Compile for Ast {
    fn compile(&self) -> compile::Output {
        let mut output = vec![];
        for node in &self.nodes {
            output.extend(node.compile());
        }
        output
    }
}

impl Compile for AstNode {
    fn compile(&self) -> compile::Output {
        match self {
            AstNode::Expr(e) => e.compile(),
        }
    }
}

impl Compile for Expr {
    fn compile(&self) -> compile::Output {
        let mut out = vec![];
        match self {
            Expr::Int(n) => self.add(&mut out, OpCode::OpNumber(*n)),
            Expr::Float(_) => todo!(),
            Expr::String(_) => todo!(),
            Expr::Ident(_) => todo!(),
            Expr::Binary { op, left, right } => {
                out.extend(left.compile());
                out.extend(right.compile());
                self.add(&mut out, op.as_opcode());
            },
            Expr::Unary { expr } => {
                expr.compile();
                self.add(&mut out, OpCode::OpNegate);
            },
        }
        out
    }
}
