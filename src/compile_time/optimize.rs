use super::ast::{AstNode, expr::{Expr, Operator}, Ast};

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
        }
    }
}

macro_rules! bin_op {
    ($self: expr, $left: expr, $right: expr, $op: tt) => {
        match ($left.as_ref(), $right.as_ref()) {
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
                        match (left.as_ref(), right.as_ref()) {
                            (Expr::Int(a), Expr::Int(b)) => 
                                *self = Expr::Int(a + b),
                            (Expr::Float(a), Expr::Float(b)) => 
                                *self = Expr::Float(a + b),
                            (Expr::String(a), Expr::String(b)) => 
                                *self = Expr::String(a.to_string() + &b),
                            _ => (),
                        }
                        
                    },
                    Operator::Minus => bin_op!(self, left, right, -),
                    Operator::Star => bin_op!(self, left, right, *),
                    Operator::Slash => bin_op!(self, left, right, /),
                    Operator::Percent => bin_op!(self, left, right, %),
                }
            },
            _ => (),
        };
    }
}
