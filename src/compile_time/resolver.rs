use std::collections::HashMap;

use super::method_translator::translate_ast_method_calls;

use crate::{spanned::Spanned, expr_type::ExprType, call::Call, apla_std::Std, class::{Class, ParsedClass}, named_obj_container::NamedObjContainer};

use super::{util::{variable::Variable, scope::Scope, func::{Func, ParsedFunc}}, ast::{Ast, AstNode, expr::{Expr, Operator}, stmt::Stmt}, error::{CTError, CTErrorKind, report_error}};

pub type FunctionBundle = (Func, ParsedFunc);

pub struct Resolver<'a> {
    callables: HashMap<String, Box<dyn Call>>,
    scope: Scope,
    text: &'a str,
    had_error: bool,
    expected_return_type: ExprType,
}


impl<'a> Resolver<'a> {
    pub fn resolve(
        ast: &'a mut Ast, text: &'a str, functions: &'a HashMap<String, Func>,
        classes: HashMap<String, Class>,
        apla_std: Std,
    ) ->  Result<HashMap<String, Box<dyn Call>>, ()>
    {
        // translate_ast_method_calls(ast, classes)
        let mut callables = HashMap::new();
        callables.insert("std".to_string(), Box::new(apla_std) as Box<dyn Call>);
        let mut new_self = Self {
            scope: Scope::new(),
            callables,
            text,
            had_error: false,
            expected_return_type: ExprType::Null,
        };
        let functions = functions
            .into_iter()
            .map(|(name, func)| 
                (name.to_string(), (func.clone(), new_self.parse_func(func)))
            )
            .collect::<HashMap<_, _>>();

        let parsed_classes = classes.into_iter()
            .map(|(name, c)| (name, new_self.parse_class(c)))
            .collect::<HashMap<_, _>>();
        for (name, (orig_func, parsed_func)) in functions {
            new_self.scope.add_scope();
            for arg in parsed_func.args.iter() {
                new_self.scope.add_var(&arg.0, arg.1.clone());
            }
            new_self.expected_return_type = parsed_func.return_type.clone();
            new_self.callables.insert(name.to_string(), Box::new(parsed_func) as Box<dyn Call>);
            translate_ast_method_calls(ast, &parsed_classes);
            new_self.resolve_node(&orig_func.node);
            new_self.scope.pop_scope();
            new_self.expected_return_type = ExprType::Null;
        };

        translate_ast_method_calls(ast, &parsed_classes);
        for (name, class) in parsed_classes {
            new_self.scope.add_scope();
            for (_, method) in &class.methods {
                for (name, var) in method.args.iter() {
                    new_self.scope.add_var(&name, var.clone());
                }
                new_self.expected_return_type = method.return_type.clone();
                new_self.resolve_node(&method.orig_node);
                new_self.expected_return_type = ExprType::Null;
            }
            new_self.callables.insert(name, Box::new(class) as Box<dyn Call>);
            new_self.scope.pop_scope();
        }
        for node in &ast.nodes {
            new_self.resolve_node(node);
        }
        
        match new_self.had_error {
            true => Err(()),
            false => Ok(new_self.callables),
        }
    }

    fn parse_class(&mut self, class: Class) -> ParsedClass {
        let mut parsed_class = ParsedClass::new(class.name);
        parsed_class.fields = class.fields
            .into_iter()
            .map(|node| self.create_var_from_node(&node))
            .map(|(name, var)| (name.to_string(), var))
            .collect();
        parsed_class.methods = class.methods
            .into_iter()
            .map(|(name, method)| (name, self.parse_func(&method)))
            .collect();
        parsed_class
    }

    fn parse_func_args(
        &mut self, args: &Vec<AstNode>
    ) -> NamedObjContainer<Variable> 
    {
        let mut result = vec![];
        for arg in args {
            match arg {
                AstNode::Stmt(s) => match s.obj_ref() {
                    Stmt::VarCreation { name, is_mut, ty, value } => {
                        let var = self.create_var(
                            s, *is_mut, name, ty, value
                        );
                        var.1.init();
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
        let mut new_result = NamedObjContainer::new();
        for (name, var) in result {
            if var.ty == ExprType::ToBeInferred {
                self.report_error(&Spanned::new(
                    CTError::new(CTErrorKind::TypeNotAnnotated), 
                    name.start, name.len
                ));
            }
            new_result.insert(&name.to_string(), var);
        }
        new_result
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
        match expr.obj_ref() {
            Expr::This { callee: _ } => Ok(ExprType::ClassThis),
            Expr::Int(_) => Ok(ExprType::Int),
            Expr::Float(_) => Ok(ExprType::Float),
            Expr::String(_) => Ok(ExprType::String),
            Expr::Bool(_) => Ok(ExprType::Bool),
            Expr::Var(n) => {
                let var = self.get_var(n, expr)?;
                if !var.is_init() {
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
                        Ok(f.get_return_type(expr))
                    }
                    None => Err(
                            Spanned::from_other_span(
                                CTError::new(
                                    CTErrorKind::FuncDoesntExist(name.to_string())
                                ), name)
                        ),
                }
            },
            Expr::Index { object, i } => {
                let object = match &**object {
                    AstNode::Stmt(_) => panic!(),
                    AstNode::Expr(e) => e,
                };
                let expr_type = self.resolve_expr(object)?;
                let tp_from_index = match expr_type.is_indexable() {
                    Some(tp) => tp,
                    None => return Err(Spanned::from_other_span(
                        CTError::new(CTErrorKind::CantIndexType(expr_type)), 
                        object)),
                };
                let index_type = self.resolve_expr(&i)?;
                if index_type != ExprType::Int {
                    return Err(Spanned::from_other_span(
                        CTError::new(CTErrorKind::MismatchedTypes(ExprType::Int, index_type)), 
                    i))
                }
                return Ok(tp_from_index)
            },
            Expr::Get { left, right } => {
                let left_span = left.just_span_data();
                let left = self.resolve_expr(&left)?;
                let obj = match &left {
                    ExprType::Class(a) => a,
                    ExprType::Pointer { points_to, is_mut: _ } => {
                        match points_to.as_ref() {
                            ExprType::Class(a) => a,
                            _ => return Err(Spanned::from_other_span(
                                CTError::new(CTErrorKind::CantUseGet), &left_span))
                        }
                    },
                    _ => {
                        return Err(Spanned::from_other_span(
                            CTError::new(CTErrorKind::CantUseGet), &left_span))
                    }
                };
                match right.obj_ref() {
                    Expr::Var(v) => {
                        match obj.fields.get(v) {
                            Some(v) => Ok(v.ty.clone()),
                            None => Err(Spanned::from_other_span(
                                CTError::new(CTErrorKind::VarDoesntExist(v.to_string())), 
                                &right))
                        }
                    },
                    Expr::Call { name: _, args: _ } => todo!(),
                    _ => panic!(),
                }
            },
            Expr::Poison => Err(Spanned::new(
                CTError::new(CTErrorKind::Poisoned), 0, 0
            )),
            Expr::Deref { expr } => {
                match self.resolve_expr(expr)?.derefed() {
                    Ok(v) => Ok(v),
                    Err(er) => Err(Spanned::from_other_span(
                        CTError::new(er), expr
                    ))
                }
            }
            Expr::MakePointer { expr, is_mut } => {
                match expr.obj_ref() {
                    Expr::Var(v) => {
                        let var = self.get_var(&v, &expr)?;
                        if *is_mut && !var.is_mut {
                            return Err(Spanned::from_other_span(
                                CTError::new(CTErrorKind::CantTakeMutPtrOfConst), 
                                expr));
                        }
                    },
                    _ => (),
                };
                Ok(self.resolve_expr(expr)?.as_pointer(*is_mut))
            }
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
                    Ok(var) => {
                        let ty = {
                            var.init();
                            match self.resolve_expr(&left) {
                                Ok(t) => t,
                                Err(er) => {
                                    self.report_error(&er);
                                    return;
                                }
                            }
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
            },
            Stmt::Return { val } => {
                match val {
                    Some(expr) => {
                        match self.resolve_expr(expr) {
                            Ok(ty) => if ty != self.expected_return_type {
                                self.report_error(&Spanned::from_other_span(
                                    CTError::new(CTErrorKind::MismatchedTypes(
                                        self.expected_return_type.clone(), ty)),
                                    expr
                                ));
                            },
                            Err(er) => self.report_error(&er),
                        }
                    },
                    None => {
                        if self.expected_return_type != ExprType::Null {
                            self.report_error(&Spanned::from_other_span(
                                CTError::new(CTErrorKind::ExpectedExpr),
                                stmt
                            ));
                        }
                    }
                }
            },
            Stmt::While { condition, body } => {
                if let Err(er) = self.resolve_expr(condition) {
                    self.report_error(&er);
                }
                self.resolve_node(&body);
            }
            Stmt::Poison | Stmt::Break | Stmt::Continue => (),
        }  
    }

    pub fn create_var_from_node(&mut self, node: &AstNode) -> (Spanned<String>, Variable) {
        match node {
            AstNode::Stmt(s) => match s.obj_ref() {
                Stmt::VarCreation { is_mut, name, ty, value } => 
                    self.create_var(s, *is_mut, name, ty, value),
                _ => panic!(),
            }
            AstNode::Expr(_) => panic!(),
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

    fn is_assignable(&self, expr: &Spanned<Expr>) -> Result<&Variable, Spanned<CTError>> {
        match expr.obj_ref() {
            Expr::Var(n) => {
                let var = self.get_var(n, expr)?;
                if !var.is_mut && var.is_init() {
                    return Err(Spanned::new(
                        CTError::new(CTErrorKind::CantAssignToConst),
                        expr.start, expr.len
                    ));
                }
                Ok(var)
            },
            Expr::Index { object, i: _ } => {
                let object = match object.as_ref() {
                    AstNode::Expr(e) => e,
                    _ => panic!(),
                };
                self.is_assignable(object)
            },
            Expr::Get { left, right } => {
                let obj = match left.obj_ref() {
                    Expr::Var(v) => match &self.get_var(&v, &left)?.ty {
                        ExprType::Class(c) => c,
                        ExprType::Pointer { points_to, is_mut: _ } => {
                            match points_to.as_ref() {
                                ExprType::Class(c) => c,
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                    Expr::Get { left: _, right: _ } => todo!(),
                    _ => panic!(),
                };
                match right.obj_ref() {
                    Expr::Var(v) => Ok(obj.fields.get(v).unwrap()),
                    Expr::Call { name: _, args: _ } => Err(
                        Spanned::from_other_span(
                            CTError::new(CTErrorKind::CantAssignToThis), right),
                    ),
                    _ => panic!(),
                }
            },
            Expr::Deref { expr } => {
                match expr.obj_ref() {
                    Expr::Var(v) => {
                        let var = self.get_var(&v, expr)?;
                        match var.ty {
                            ExprType::Pointer { points_to: _, is_mut } => {
                                if !is_mut {
                                    return Err(Spanned::from_other_span(
                                        CTError::new(CTErrorKind::CantAssignToConstPtr),
                                        &expr
                                    ));
                                }
                            },
                            _ => todo!(),
                        }
                        self.get_var(v, &expr)
                    },
                    _ => todo!(),
                }
            }
            _ => Err(Spanned::new(
                CTError::new(CTErrorKind::ExpectedPlace),
                expr.start, expr.len,
            )),
        }
    }

    pub fn report_error(&mut self, error: &Spanned<CTError>) {
        self.had_error = true;
        report_error(error, self.text);
    }

    fn get_callable(&self, name: &str) -> Option<&dyn Call> {
        if name.contains('.') {
            Some(self.get_method(name))
        } else {
            self.get_boxed_callable(name)
                .map(|callable| callable.as_ref())
        }
    }

    fn get_boxed_callable(&self, name: &str) -> Option<&Box<dyn Call>> {
        self.callables.get(name)
    }

    // FORMAT: 'foo.bar()' in source code is 'Foo.bar(foo)' here
    fn get_method(&self, name: &str) -> &dyn Call {
        let parts = name.split('.').collect::<Vec<_>>();
        if parts.len() != 2 { panic!() }
        let (class_name, method) = (parts[0], parts[1]);
        self.callables.get(class_name).unwrap().as_obj().unwrap()
            .methods
            .get(method)
            .map(|method| method as &dyn Call)
            .unwrap()
    }

    fn get_var<T>(
        &self, name: &str, span_for_err: &Spanned<T>
    ) -> Result<&Variable, Spanned<CTError>> 
    {
        match self.scope.get_var(name) {
            Ok(var) => Ok(var),
            Err(kind) => Err(Spanned::from_other_span(
                CTError::new(kind), span_for_err
            )),
        }
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
