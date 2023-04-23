use std::collections::HashMap;

use crate::{spanned::Spanned, expr_type::ExprType, call::Call, apla_std::Std};

use super::{util::{variable::Variable, scope::Scope, func::{Func, ParsedFunc}}, ast::{Ast, AstNode, expr::{Expr, Operator}, stmt::Stmt}, error::{CTError, CTErrorKind, report_error}};

pub type FunctionBundle = (Func, ParsedFunc);

pub struct Resolver<'a> {
    ast: &'a Ast,
    callables: HashMap<String, Box<dyn Call>>,
    scope: Scope,
    text: &'a str,
    had_error: bool,
}


impl<'a> Resolver<'a> {
    pub fn resolve(
        ast: &'a Ast, text: &'a str, functions: &'a HashMap<String, Func>,
        apla_std: Std,
    ) ->  Result<HashMap<String, Box<dyn Call>>, ()>
    {
        let mut callables = HashMap::new();
        callables.insert("std".to_string(), Box::new(apla_std) as Box<dyn Call>);
        let mut new_self = Self {
            ast,
            scope: Scope::new(),
            callables: callables,
            text,
            had_error: false,
        };
        let functions = functions
            .into_iter()
            .map(|(name, func)| 
                (name.to_string(), (func.clone(), new_self.parse_func(func)))
            )
            .collect::<HashMap<_, _>>();
        for (name, (orig_func, parsed_func)) in functions {
            new_self.scope.add_scope();
            for arg in &parsed_func.args {
                new_self.scope.add_var(arg.0, arg.1.clone());
            }
            new_self.resolve_node(&orig_func.node);
            new_self.scope.pop_scope();
            new_self.callables.insert(name.to_string(), Box::new(parsed_func) as Box<dyn Call>);
        };
        // doing it here doesn't require borrowing self
        // resolving
        for node in &new_self.ast.nodes {
            new_self.resolve_node(node);
        }
        match new_self.had_error {
            true => Err(()),
            false => Ok(new_self.callables),
        }
    }

    fn parse_func_args(
        &mut self, args: &Vec<AstNode>
    ) -> HashMap<String, Variable> 
    {
        let mut result = vec![];
        for arg in args {
            match arg {
                AstNode::Stmt(s) => match s.obj_ref() {
                    Stmt::VarCreation { name, is_mut, ty, value } => {
                        let mut var = self.create_var(
                            s, *is_mut, name, ty, value
                        );
                        var.1.initialized = true;
                        result.push(var)
                    },
                    _ => {
                        self.report_error(&Spanned::new(
                            CTError::new(CTErrorKind::ExpectedVarCreation),
                            s.start, s.len
                        ));
                    }
                },
                AstNode::Expr(e) => {
                    self.report_error(&Spanned::new(
                        CTError::new(CTErrorKind::ExpectedStmt),
                        e.start, e.len
                    ));
                }
            }
        };
        result.into_iter()
            .map(|(name, var)| {
                if var.ty == ExprType::ToBeInferred {
                    self.report_error(&Spanned::new(
                        CTError::new(CTErrorKind::TypeNotAnnotated), 
                        name.start, name.len
                    ));
                }
                (name.to_string(), var)
            }).collect()
    }

    fn parse_func(&mut self, func: &Func) -> ParsedFunc {
        let args = self.parse_func_args(&func.args);
        let ret_type = match ExprType::try_from(func.return_type.as_ref()) {
            Ok(ty) => {
                if ty == ExprType::ToBeInferred {
                    self.report_error(&Spanned::new(
                        CTError::new(CTErrorKind::TypeNotAnnotated), 
                        func.return_type.start, func.return_type.len
                    ));
                }
                ty
            },
            Err(_) => {
                self.report_error(&Spanned::from_other_span(
                    CTError::new(CTErrorKind::ExpectedType),
                    &func.return_type,
                ));
                ExprType::Null
            },
        };
        ParsedFunc::new(func.name.to_string(), ret_type, args, func.node.clone())
    }

    pub fn resolve_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Expr(e) => {
                if let Err(er) = self.resolve_expr(e) {
                    self.report_error(&er);
                };
            },
            AstNode::Stmt(s) => {self.resolve_stmt(s);},
        };
    }

    pub fn resolve_expr(&self, expr: &Spanned<Expr>) -> Result<ExprType, Spanned<CTError>> {
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
                let t1 = self.resolve_expr(left)?;
                let t2 = self.resolve_expr(right)?;
                let allowed_types = op.get_legal_types();
                if !allowed_types.contains(&ExprType::Any) {    
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
                }
                match op {
                    Operator::Smaller | Operator::SmallerEqual | 
                    Operator::Greater | Operator::GreaterEqual | 
                    Operator::Equal | Operator::NotEqual => Ok(ExprType::Bool),
                    _ => Ok(t1),
                }
            },
            Expr::Unary { expr } => {
                match self.resolve_expr(expr)? {
                    t @ (ExprType::Int | ExprType::Float |
                    ExprType::Bool) =>  Ok(t),
                    t => Err(make_error(CTErrorKind::CantNegateType(t), 
                        expr.start, expr.len)),
                }
            },
            Expr::Call { name, args: _ } => {
                match self.get_callable(&name) {
                    Some(f) => {
                        f.resolve(expr, self)?;
                        Ok(f.get_return_type())
                    }
                    None => Err(
                            Spanned::from_other_span(
                                CTError::new(
                                    CTErrorKind::FuncDoesntExist(name.to_string())
                                ), name)
                        ),
                }
            }
            Expr::Poison => Err(Spanned::new(
                CTError::new(CTErrorKind::Poisoned), 0, 0
            )),
        }
    }

    pub fn resolve_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match stmt.obj_ref() {
            Stmt::VarCreation { is_mut, name, ty, value } => {
                let (name, var) = self.create_var(
                    stmt, *is_mut, name, ty, value
                );
                self.scope.add_var(&name, var)
            },
            Stmt::Assignment { left, right } => {
                let ty1 = match self.resolve_expr(right) {
                    Ok(t) => t,
                    Err(er) => {
                        self.report_error(&er);
                        ExprType::ToBeInferred
                    },
                };
                
                match self.is_assignable(left) {
                    Ok(name) => {
                        let ty = {
                            let v = self.scope.get_var_mut(&name).unwrap();
                            v.initialized = true;
                            v.ty.clone()
                        };
                        if ty != ty1 {
                            self.report_error(&Spanned::new(
                                CTError::new(CTErrorKind::MismatchedTypes(
                                    ty, ty1)),
                                left.start, left.len
                            ));
                        }
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
                    self.resolve_node(node);
                }
                self.scope.pop_scope();
            },
            Stmt::If { condition, true_branch, false_branch } => {
                let cond_type = match self.resolve_expr(condition) {
                    Ok(ty) => ty,
                    Err(er) => {
                        self.report_error(&er);
                        ExprType::Any
                    }
                };
                if cond_type != ExprType::Bool && cond_type != ExprType::Any {
                    self.report_error(&Spanned::new(CTError::new(
                        CTErrorKind::MismatchedTypes(ExprType::Bool, cond_type)
                    ), condition.start, condition.len))
                }
                self.resolve_node(true_branch);
                if let Some(node) = false_branch {
                    self.resolve_node(node);
                }
            }
            Stmt::Poison => (),
        }  
    }

    pub fn create_var(
        &mut self, stmt: &Spanned<Stmt>,
        is_mut: bool, name: &Spanned<String>,
        ty: &Spanned<String>, value: &Option<Spanned<Expr>>
    ) -> (Spanned<String>, Variable)
    {
        let mut poisoned = false;
        let init = value.is_some();
        let var_type;
        if value.is_none() {
            match ExprType::try_from(ty as &str) {
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
            match ExprType::try_from(ty as &str) {
                Ok(mut t) => {
                    match self.resolve_expr(value) {
                        Ok(t2) => {
                            if t == ExprType::ToBeInferred {
                                t = t2;
                            } else if t != t2 {
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
        let mut var = Variable::new(var_type, is_mut, init);
        if poisoned { var.poison() }
        (name.clone(), var)
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

    pub fn report_error(&mut self, error: &Spanned<CTError>) {
        self.had_error = true;
        report_error(error, self.text);
    }

    fn get_callable(&self, name: &str) -> Option<&Box<dyn Call>> {
        self.callables.get(name)
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
