use crate::run_time::bytecode::OpCode;
use super::ast::stmt::Stmt;
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
            AstNode::Stmt(e) => e.compile(),
        }
    }
}

use OpCode::*; // OpCodes are prefixed with Op
impl Compile for Expr {
    fn compile(&self) -> compile::Output {
        let mut out = vec![];
        match self {
            Expr::Int(n) => self.add(&mut out, OpNumber(*n)),
            Expr::Float(f) => self.add(&mut out, OpFloat(*f)),
            Expr::String(s) => self.add(&mut out, OpString(s.to_string())),
            Expr::Bool(b) => self.add(&mut out, OpBool(*b)),
            Expr::Var(name) => {
                self.add(&mut out, OpGetVar(name.to_string()))
            }
            Expr::Binary { op, left, right } => {
                out.extend(left.compile());
                out.extend(right.compile());
                self.add(&mut out, op.as_opcode());
            },
            Expr::Unary { expr } => {
                out.extend(expr.compile());
                self.add(&mut out, OpCode::OpNegate);
            },
            Self::Poison => panic!(),
        }
        out
    }
}

impl Compile for Stmt {
    fn compile(&self) -> compile::Output {
        let mut out = vec![];
        match self {
            Stmt::VarCreation { is_mut: _, name, ty: _, value } => {
                if let Some(val) = value {
                    out.extend(val.compile());
                    // place expression
                    self.add(&mut out, OpCreateVar(name.to_string()));
                    self.add(&mut out, OpGetVar(name.to_string()));
                    self.add(&mut out, OpSet);
                    return out;
                }
                self.add(&mut out, OpCreateVar(name.to_string()));
            },
            Stmt::Assignment { left, right } => {
                out.extend(right.compile());
                out.extend(left.compile());
                self.add(&mut out, OpSet);
            },
            Stmt::Block { nodes } => {
                self.add(&mut out, OpPushScope);
                for node in nodes {
                    out.extend(node.compile());
                }
                self.add(&mut out, OpPopScope);
            },
            Stmt::If { condition, true_branch, false_branch } => {
                self.add(&mut out, OpPushScope);
                out.extend(condition.compile());
                let true_branch_code = true_branch.compile();
                let true_branch_len = true_branch_code.len();
                self.add(&mut out, OpIf(true_branch_len));
                out.extend(true_branch_code);
                if let Some(branch) = false_branch {
                    let false_branch_code = branch.compile();
                    let false_branch_len = false_branch_code.len();
                    self.add(&mut out, OpElse(false_branch_len));
                    out.extend(false_branch_code);
                }
                self.add(&mut out, OpPopScope);
            }
            Stmt::Poison => panic!(),
        }
        out
    }
}
