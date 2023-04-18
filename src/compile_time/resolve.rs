use crate::{spanned::Spanned, expr_type::ExprType};

use super::{error::{CTError, report_error, CTErrorKind}, ast::{Ast, AstNode, expr::Expr}};
    
fn make_error(
    kind: CTErrorKind, start: usize, len: usize,
) -> Option<Spanned<CTError>> 
{
    Some(Spanned {
        obj: CTError::new(kind),
        start,
        len,
        poisoned: false,
    })
}


pub trait Resolve {
    type OkOutput;

    fn resolve(&mut self) -> Result<Self::OkOutput, Option<Spanned<CTError>>>;

}

impl Resolve for Ast {
    type OkOutput = ();
    fn resolve(&mut self) -> Result<Self::OkOutput, Option<Spanned<CTError>>> {
        let mut had_error = false;
        for node in &mut self.nodes {
            if let Err(er) = node.resolve() {
                had_error = true;
                if let Some(er) = er {
                    if let Some(text) = &self.text {
                        report_error(&er, text)
                    }
                }
            }
        }
        if !had_error { Ok(()) } else { 
            Err(make_error(CTErrorKind::HadError, 0, 0))
        }
    }
}

impl Resolve for AstNode {
    type OkOutput = ();
    fn resolve(&mut self) -> Result<Self::OkOutput, Option<Spanned<CTError>>> {
        match self {
            AstNode::Expr(e) => e.resolve()?,
        };
        Ok(())
    }
}

impl Resolve for Spanned<Expr> {
    type OkOutput = ExprType;
    fn resolve(&mut self) -> Result<Self::OkOutput, Option<Spanned<CTError>>> {
        if self.poisoned { return Ok(ExprType::ToBeInferred) }
        match &mut **self {
            Expr::Int(_) => Ok(ExprType::Int),
            Expr::Float(_) => Ok(ExprType::Float),
            Expr::String(_) => Ok(ExprType::String),
            Expr::Bool(_) => Ok(ExprType::Bool),
            Expr::Ident(_) => todo!(),
            Expr::Binary { op, left, right } => {
                let t1 = left.resolve()?;
                let t2 = right.resolve()?;
                let allowed_types = op.get_legal_types();
                if allowed_types.contains(&ExprType::Any) {
                    return Ok(t1);
                }
                if t1 != t2 {
                    return Err(make_error(
                        CTErrorKind::MismatchedTypes(t1, t2),
                        self.start, self.len,
                    ));
                }
                if !allowed_types.contains(&t1) {
                    return Err(make_error(
                        CTErrorKind::CantUseOpForTypes(op.clone(), t1),
                        self.start, self.len,
                    ));
                }
                Ok(t1)
            },
            Expr::Unary { expr } => {
                match expr.resolve()? {
                    t @ (ExprType::Int | ExprType::Float |
                    ExprType::Bool) =>  Ok(t),
                    t @ _ => Err(make_error(CTErrorKind::CantNegateType(t), 
                        self.start, self.len)),
                }
            },
        }
    }
}
