use super::ast::{AstNode, expr::{Expr, Operator}, Ast};

pub trait Optimize {
    fn try_to_optimize(&mut self) -> Option<Self>
    where
        Self: Sized;

    fn optimize(&mut self) 
    where
        Self: Sized,
    {
        if let Some(optimized_self) = self.try_to_optimize() {
            *self = optimized_self;
        }
    }
}

impl Ast {
    pub fn optimize(&mut self) {
        for node in &mut self.nodes {
            node.optimize()
        }
    }
}

impl Optimize for AstNode {
    fn try_to_optimize(&mut self) -> Option<Self> {
        match self {
            AstNode::Expr(e) => Some(AstNode::Expr(e.try_to_optimize()?)),
        }
    }
}

macro_rules! bin_op {
    ($left: expr, $right: expr, $op: tt) => {
        match ($left.as_ref(), $right.as_ref()) {
            (Expr::Int(a), Expr::Int(b)) => Some(Expr::Int(a $op b)),
            (Expr::Float(a), Expr::Float(b)) => Some(Expr::Float(a $op b)),
            _ => None,
        }
    };
}

impl Optimize for Expr {
    fn try_to_optimize(&mut self) -> Option<Self> {
        match self {
            Expr::Binary { op, left, right } => {
                left.optimize();
                right.optimize();
                match op {
                    Operator::Plus => {
                        match (left.as_ref(), right.as_ref()) {
                            (Expr::Int(a), Expr::Int(b)) => 
                                Some(Expr::Int(a + b)),
                            (Expr::Float(a), Expr::Float(b)) => 
                                Some(Expr::Float(a + b)),
                            (Expr::String(a), Expr::String(b)) => 
                                Some(Expr::String(a.to_string() + &b)),
                            _ => None,
                        }
                        
                    },
                    Operator::Minus => bin_op!(left, right, -),
                    Operator::Star => bin_op!(left, right, *),
                    Operator::Slash => bin_op!(left, right, /),
                    Operator::Percent => bin_op!(left, right, %),
                }
            },
            _ => None,
        }
    }
}
