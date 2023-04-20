use crate::{spanned::Spanned, expr_type::ExprType};

use super::{util::{variable::Variable, scope::Scope}, ast::{Ast, AstNode, expr::Expr, stmt::Stmt}, error::{CTError, CTErrorKind, report_error}};

pub struct Resolver<'a> {
    ast: &'a Ast,
    scope: Scope,
    text: &'a str,
    had_error: bool,
}

macro_rules! report_if_err {
    ($self: expr, $($action: expr)+) => {
        $(
            if let Err(er) = $action {
                $self.report_error(&er);
            }
        )*
    };
}


impl<'a> Resolver<'a> {
    pub fn new(ast: &'a Ast, text: &'a str) -> Self {
        Self {
            ast,
            scope: Scope::new(),
            text,
            had_error: false,
        }
    }

    pub fn resolve(&mut self) -> Result<(), ()> {
        for node in &self.ast.nodes {
            self.resolve_node(node);
        }
        println!("VARIABLES: {:?}", self.scope);
        match self.had_error {
            true => Err(()),
            false => Ok(()),
        }
    }

    fn resolve_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Expr(e) => {
                if let Err(er) = self.resolve_expr(&e) {
                    self.report_error(&er);
                };
                ()
            },
            AstNode::Stmt(s) => {self.resolve_stmt(&s);()},
        };
    }

    fn resolve_expr(&self, expr: &Spanned<Expr>) -> Result<ExprType, Spanned<CTError>> {
        if expr.poisoned { return Ok(ExprType::ToBeInferred) }
        match &**expr {
            Expr::Int(_) => Ok(ExprType::Int),
            Expr::Float(_) => Ok(ExprType::Float),
            Expr::String(_) => Ok(ExprType::String),
            Expr::Bool(_) => Ok(ExprType::Bool),
            Expr::Var(n) => {
                let var = match self.scope.get_var(n) {
                    Ok(var) => var,
                    Err(kind) => {
                        return Err(make_error(kind, expr.start, expr.len))
                    }
                };
                if !var.initialized {
                    return Err(
                        make_error(
                            CTErrorKind::UninitVarUsed(n.to_string()), 
                            expr.start, expr.len
                    ));
                }
                Ok(var.ty.clone())
            },
            Expr::Binary { op, left, right } => {
                let t1 = self.resolve_expr(&left)?;
                let t2 = self.resolve_expr(&right)?;
                let allowed_types = op.get_legal_types();
                if allowed_types.contains(&ExprType::Any) {
                    return Ok(t1);
                }
                if t1 != t2 {
                    return Err(make_error(
                        CTErrorKind::MismatchedTypes(t1, t2),
                        expr.start, expr.len,
                    ));
                }
                if !allowed_types.contains(&t1) {
                    return Err(make_error(
                        CTErrorKind::CantUseOpForTypes(op.clone(), t1),
                        expr.start, expr.len,
                    ));
                }
                Ok(t1)
            },
            Expr::Unary { expr } => {
                match self.resolve_expr(expr)? {
                    t @ (ExprType::Int | ExprType::Float |
                    ExprType::Bool) =>  Ok(t),
                    t @ _ => Err(make_error(CTErrorKind::CantNegateType(t), 
                        expr.start, expr.len)),
                }
            },
            Expr::Poison => Err(Spanned::new(
                CTError::new(CTErrorKind::Poisoned), 0, 0
            )),
        }
    }

    fn resolve_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match stmt.obj_ref() {
            Stmt::VarCreation { is_mut, name, ty, value } => {
                let mut poisoned = false;
                let init = value.is_some();
                let var_type;
                if value.is_none() {
                    match ExprType::try_from(&ty as &str) {
                        Ok(t) => {
                            if t == ExprType::ToBeInferred {
                                self.report_error(&Spanned::new(CTError::new(
                                    CTErrorKind::CantInferType), 
                                    stmt.start, stmt.len
                                ))
                            }
                            var_type = t;
                        },
                        Err(er) => {
                            self.report_error(&Spanned::new(er, stmt.start, stmt.len));
                            poisoned = true;
                            var_type = ExprType::ToBeInferred;
                        },
                    }
                } else {
                    let value = value.as_ref().unwrap();
                    match ExprType::try_from(&ty as &str) {
                        Ok(mut t) => {
                            match self.resolve_expr(&value) {
                                Ok(t2) => {
                                    if t == ExprType::ToBeInferred {
                                        t = t2;
                                    } else if &t != &t2 {
                                        poisoned = true;
                                        self.report_error(&Spanned::new(CTError::new(
                                            CTErrorKind::MismatchedTypes(t.clone(), t2)), 
                                            value.start, value.len
                                        ));
                                    }
                                    
                                },
                                Err(er) =>{
                                    self.report_error(&er);
                                    poisoned = true;
                                }
                            }
                            var_type = t
                        },
                        Err(er) => {
                            self.report_error(&Spanned::new(er, ty.start, ty.len));
                            poisoned = true;
                            var_type = ExprType::Int
                        },
                    };
                }   
                let mut var = Variable::new(var_type, *is_mut, init);
                if poisoned { var.poison() }
                self.scope.add_var(name, var);
            },
            Stmt::Assignment { left, right } => {
                report_if_err!(self, self.resolve_expr(right));
                
                match self.is_assignable(&left) {
                    Ok(name) => {
                        self.scope.get_var_mut(&name).unwrap().initialized = true;
                    },
                    Err(er) => self.report_error(&er),
                }
                if let Err(er) = self.resolve_expr(left) {
                    match er.kind {
                        CTErrorKind::UninitVarUsed(_) => (),
                        _ => self.report_error(&er),
                    }
                }
            },
            Stmt::Block { nodes } => {
                self.scope.add_scope();
                for node in nodes {
                    self.resolve_node(&node);
                }
                self.scope.pop_scope();
            },
            Stmt::Poison => (),
        }  
    }

    fn is_assignable(&self, expr: &Spanned<Expr>) -> Result<String, Spanned<CTError>> {
        if !expr.is_place() {
            return Err(Spanned::new(
                CTError::new(CTErrorKind::ExpectedPlace),
                expr.start, expr.len,
            ));
        }
        let var_name;
        match expr.obj_ref() {
            Expr::Var(n) => {
                var_name = n.to_string();
                let var = match self.scope.get_var(n) {
                    Ok(var) => var,
                    Err(kind) => return Err(Spanned::new(
                        CTError::new(kind), expr.start, expr.len
                    )),
                };
                if !var.is_mut && var.initialized {
                    return Err(Spanned::new(
                        CTError::new(CTErrorKind::CantAssignToConst),
                        expr.start, expr.len
                    ));
                }
            },
            _ => unreachable!("not places"),
        }
        Ok(var_name)
    }

    fn report_error(&mut self, error: &Spanned<CTError>) {
        self.had_error = true;
        report_error(error, self.text);
    }
}


fn make_error(
    kind: CTErrorKind, start: usize, len: usize,
) -> Spanned<CTError>
{
    Spanned {
        obj: CTError::new(kind),
        start,
        len,
        poisoned: false,
    }
}

