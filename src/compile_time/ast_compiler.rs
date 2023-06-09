use std::collections::HashMap;

use crate::call::Call;
use crate::run_time::bytecode::OpCode;
use super::ast::stmt::Stmt;
use super::ast::{Ast, expr::Expr, AstNode};

use super::compile::{self, Compile, Ctx};
use super::util::func::{Func};

pub struct Compiler<'a> {
    pub ast: &'a Ast,
    pub callables: HashMap<String, Box<dyn Call>>
}

impl<'a> Compiler<'a> {
    pub fn new(
        ast: &'a Ast, 
        callables: HashMap<String, Box<dyn Call>>
    ) -> Self 
    {
        Self { ast, callables }
    }

    pub fn compile(self) -> (compile::Output, HashMap<String, Box<dyn Call>>) 
    {
        let bytecode = {
            let ctx = Ctx {
                callables: &self.callables,
            };
            for callable in ctx.callables.values() {
                callable.compile_to_code(&ctx);
            }
            // for class in self.classes.values() {
            //     for method in class.methods.values() {
            //         method.code.replace(method.orig_node.compile(&ctx));
            //     }
            // }
            self.ast.compile(&ctx)
        };
        (bytecode, self.callables)
    }
}

impl Compile for Func {
    fn compile(&self, ctx: &Ctx) -> compile::Output {
        self.node.compile(ctx)
    }
}

impl Compile for Ast {
    fn compile(&self, ctx: &Ctx) -> compile::Output {
        let mut output = vec![];
        for node in &self.nodes {
            output.extend(node.compile(ctx));
        }
        output
    }
}

impl Compile for AstNode {
    fn compile(&self, ctx: &Ctx) -> compile::Output {
        match self {
            AstNode::Expr(e) => e.compile(ctx),
            AstNode::Stmt(e) => e.compile(ctx),
        }
    }
}

use OpCode::*; // OpCodes are prefixed with Op
impl Compile for Expr {
    fn compile(&self, ctx: &Ctx) -> compile::Output {
        let mut out = vec![];
        match self {
            Expr::This { callee }=> {
                out.extend(callee.compile(ctx));
                self.add(&mut out, OpMakePointer);
            },
            Expr::Int(n) => self.add(&mut out, OpNumber(*n)),
            Expr::Float(f) => self.add(&mut out, OpFloat(*f)),
            Expr::String(s) => self.add(&mut out, OpString(s.to_string())),
            Expr::Bool(b) => self.add(&mut out, OpBool(*b)),
            Expr::Var(name) => {
                self.add(&mut out, OpGetVar(name.to_string()))
            }
            Expr::Binary { op, left, right } => {
                out.extend(left.compile(ctx));
                out.extend(right.compile(ctx));
                self.add(&mut out, op.as_opcode());
            },
            Expr::Unary { expr } => {
                out.extend(expr.compile(ctx));
                self.add(&mut out, OpCode::OpNegate);
            },
            Expr::Call { name, args } => {
                out.extend(ctx.get(name.obj_ref())
                    .compile_call(args, ctx));
                // this is so hacky and so bad
                let opcode = match out.iter_mut().rev().nth(1) {
                    Some(o) => o,
                    None => return out,
                };
                // Call Foo.bar() instead of bar()
                match opcode {
                    OpCall(n) => *n = name.to_string(),
                    _ => (),
                };
            },
            Expr::Index { object, i } => {
                out.extend(object.compile(ctx));
                out.extend(i.compile(ctx));
                self.add(&mut out, OpCode::OpIndex);
            },
            Expr::Get { left, right } => {
                out.extend(left.compile(ctx));
                match right.obj_ref() {
                    Expr::Var(v) => self.add(&mut out, OpGetField(v.to_string())),
                    Expr::Call { name, args } => {
                        out.extend(ctx.callables.get(name.obj_ref())
                        .unwrap()
                        .compile_call(args, ctx));
                        self.add(&mut out, OpGetField(name.to_string()));
                    }
                    _ => todo!(),
                }
            },
            Expr::MakePointer { expr, is_mut: _ } => {
                out.extend(expr.compile(ctx));
                self.add(&mut out, OpCode::OpMakePointer);
            },
            Expr::Deref { expr } => {
                out.extend(expr.compile(ctx));
                self.add(&mut out, OpCode::OpDeref);
            }
            Self::Poison => panic!(),
        }
        out
    }
}

impl Compile for Stmt {
    fn compile(&self, ctx: &Ctx) -> compile::Output {
        let mut out = vec![];
        match self {
            Stmt::VarCreation { is_mut: _, name, ty: _, value } => {
                if let Some(val) = value {
                    out.extend(val.compile(ctx));
                    // place expression
                    self.add(&mut out, OpCreateVar(name.to_string()));
                    self.add(&mut out, OpGetVar(name.to_string()));
                    self.add(&mut out, OpSet);
                    return out;
                }
                self.add(&mut out, OpCreateVar(name.to_string()));
            },
            Stmt::Assignment { left, right } => {
                out.extend(right.compile(ctx));
                out.extend(left.compile(ctx));
                self.add(&mut out, OpSet);
            },
            Stmt::Block { nodes } => {
                self.add(&mut out, OpPushScope);
                for node in nodes {
                    out.extend(node.compile(ctx));
                }
                self.add(&mut out, OpPopScope);
            },
            Stmt::If { condition, true_branch, false_branch } => {
                self.add(&mut out, OpPushScope);
                out.extend(condition.compile(ctx));
                let true_branch_code = true_branch.compile(ctx);
                let true_branch_len = true_branch_code.len();
                if let Some(branch) = false_branch {
                    let false_branch_code = branch.compile(ctx);
                    let false_branch_len = false_branch_code.len();
                    // the + 1 comes from the else
                    self.add(&mut out, OpIf(true_branch_len + 1));
                    out.extend(true_branch_code);
                    self.add(&mut out, OpElse(false_branch_len));
                    out.extend(false_branch_code);
                } else {   
                    self.add(&mut out, OpIf(true_branch_len));
                    out.extend(true_branch_code);
                }
                self.add(&mut out, OpPopScope);
            },
            Stmt::Return { val } => {
                if let Some(expr) = val {
                    out.extend(expr.compile(ctx));
                }
                out.push(OpCode::OpReturn);
            },
            Stmt::While { condition, body } => {
                let cond = condition.compile(ctx);
                let body = body.compile(ctx);
                let body_len = body.len();
                let cond_len = cond.len();
                out.extend(cond);
                self.add(&mut out, OpLoop(body_len));
                out.extend(body);
                // 1 is OpLoop
                self.add(&mut out, OpEndLoop(body_len + cond_len + 1));
            },
            Stmt::Continue => self.add(&mut out, OpCode::OpContinue),
            Stmt::Break => self.add(&mut out, OpCode::OpBreak),
            Stmt::Poison => panic!(),
        }
        out
    }
}
