use super::ast::{AstNode, expr::{Expr, Operator}, Ast, stmt::Stmt};

pub trait Optimize {
    fn optimize(&mut self)
    where
        Self: Sized;
}

impl Optimize for Ast {
    fn optimize(&mut self) {
        for node in &mut self.nodes {
            node.optimize()
        }
    }
}

impl Optimize for AstNode {
    fn optimize(&mut self) {
        match self {
            AstNode::Expr(e) => e.optimize(),
            AstNode::Stmt(e) => e.optimize(),
        }
    }
}

macro_rules! bin_op {
    ($self: expr, $left: expr, $right: expr, $op: tt, bool) => {
        match ($left.obj_ref(), $right.obj_ref()) {
            (Expr::Int(a), Expr::Int(b)) => *$self = Expr::Bool(a $op b),
            (Expr::Float(a), Expr::Float(b)) => *$self = Expr::Bool(a $op b),
            _ => (),
        }
    };
    ($self: expr, $left: expr, $right: expr, $op: tt) => {
        match ($left.obj_ref(), $right.obj_ref()) {
            (Expr::Int(a), Expr::Int(b)) => *$self = Expr::Int(a $op b),
            (Expr::Float(a), Expr::Float(b)) => *$self = Expr::Float(a $op b),
            _ => (),
        }
    };
}

impl Optimize for Expr {
    fn optimize(&mut self) {
        match self {
            Expr::Binary { op, left, right } => {
                left.optimize();
                right.optimize();
                match op {
                    Operator::Plus => {
                        match (left.obj_ref(), right.obj_ref()) {
                            (Expr::Int(a), Expr::Int(b)) => 
                                *self = Expr::Int(a + b),
                            (Expr::Float(a), Expr::Float(b)) => 
                                *self = Expr::Float(a + b),
                            (Expr::String(a), Expr::String(b)) => 
                                *self = Expr::String(a.to_string() + b),
                            _ => (),
                        }
                        
                    },
                    Operator::Minus => bin_op!(self, left, right, -),
                    Operator::Star => bin_op!(self, left, right, *),
                    Operator::Slash => bin_op!(self, left, right, /),
                    Operator::Percent => bin_op!(self, left, right, %),
                    Operator::Smaller => bin_op!(self, left, right, <, bool),
                    Operator::SmallerEqual => bin_op!(self, left, right, <=, bool),
                    Operator::Greater => bin_op!(self, left, right, >, bool),
                    Operator::GreaterEqual => bin_op!(self, left, right, >=, bool),
                    Operator::Equal => bin_op!(self, left, right, ==, bool),
                    Operator::NotEqual => bin_op!(self, left, right, !=, bool),
                }
            },
            Expr::Unary { expr } => {
                match expr.obj_mut() {
                    Expr::Int(i) => *self = Expr::Int(-*i),
                    Expr::Float(f) => *self = Expr::Float(-*f),
                    Expr::Unary { expr } => {
                        expr.optimize(); // expr might be unary
                        match expr.obj_ref() {
                            Expr::Int(i) => *self = Expr::Int(*i),
                            Expr::Float(f) => *self = Expr::Float(*f),
                            _ => (),
                        }
                    },
                    Expr::Binary { .. } => {
                        expr.optimize();
                        match expr.obj_ref() {
                            Expr::Int(i) => *self = Expr::Int(*i),
                            Expr::Float(f) => *self = Expr::Float(*f),
                            _ => (),
                        }
                    }
                    _ => (),
                }
            }
            Expr::Call { name: _, args } => {
                for arg in args {
                    arg.optimize();
                }
            },
            Expr::Index { object, i } => {
                object.optimize();
                i.optimize();
            },
            Expr::Get { left, right } => {
                left.optimize();
                right.optimize();
            }
            Expr::Poison => todo!(),
            _ => (),
        };
    }
}

impl Optimize for Stmt {
    fn optimize(&mut self) {
        match self {
            Stmt::VarCreation { is_mut: _, name: _, ty: _, value } => {
                if let Some(expr) = value {
                    expr.optimize();
                }
            },
            Stmt::Assignment { left, right } => {
                left.optimize();
                right.optimize();
            },
            Stmt::Block { nodes } => {
                for node in nodes {
                    node.optimize();
                }
            },
            Stmt::If { condition, true_branch, false_branch } => {
                condition.optimize();
                true_branch.optimize();
                if let Some(br) = false_branch {
                    br.optimize();
                }
            },
            Stmt::Return { val } => {
                if let Some(expr) = val {
                    expr.optimize();
                }
            },
            Stmt::While { condition, body } => {
                condition.optimize();
                body.optimize();
            },
            
            Self::Poison | Self::Break | Self::Continue => (),
        }
    }
}
