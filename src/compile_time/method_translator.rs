// This is probably the worst solution to a problem that will ever exist.
// The final compiler can't compile a call that's "foo.bar()" because it doesn't
// know what foo is (it needs to know what are the names of the args, and each
// &dyn Call has its own way to compile),  and can't know that it's an 
// object of a Foo class.
// This is solved by walking the AST yet another time JUST to change
// all method calls from "foo.bar()" to "Foo.bar(@mut foo)"
// This step will occur in between of resolving the bodies of functions, code
// and the headers 
// (1. Class -> ParsedClass, 2. foo.bar() -> Foo.bar(@mut foo), 3. resolving code)

// If the call can't be figured out here, for example if an object of a class doesn't
// exist or the method doesn't exist, the program doesn't care about here
// and it will be resolved in the next step ("3." above)

use std::collections::HashMap;

use crate::{class::ParsedClass, call::Call, expr_type::ExprType, spanned::Spanned};

use super::{ast::{Ast, AstNode, stmt::Stmt, expr::Expr}, util::{scope::Scope, variable::Variable}};

type Classes = HashMap<String, ParsedClass>;

pub fn translate_node_method_calls(
    node: &mut AstNode, 
    classes: &Classes,
    starting_vars: Vec<(String, Variable)>,
) 
{
    let mut scope = Scope::new();
    for (name, var) in starting_vars {
        scope.add_var(&name, var);
    }
    translate_node(node, &mut scope, classes);
}

pub fn translate_ast_method_calls(ast: &mut Ast, 
    classes: &Classes,
    starting_vars: Vec<(String, Variable)>,
) 
{
    let mut scope = Scope::new();
    for (name, var) in starting_vars {
        scope.add_var(&name, var);
    }
    for node in &mut ast.nodes {
        translate_node(node, &mut scope, classes);
    }
}

fn translate_to_desired_form(node: &mut Spanned<Expr>, scope: &mut Scope) {
    let Some(new_call_node) = try_to_construct_call(node, &scope) else { return };
    let (span, new_node) = match new_call_node {
        AstNode::Expr(e) => (e.just_span_data(), e.obj),
        _ => unreachable!()
    };
    *node = Spanned::from_other_span(new_node, &span);
}

fn translate_expr(node: &mut Spanned<Expr>, scope: &mut Scope, classes: &Classes) {
    match node.obj_mut() {
        Expr::Binary { op: _, left, right } => {
            translate_expr(left, scope, classes);
            translate_expr(right, scope, classes);
        },
        Expr::Unary { expr } => translate_expr(expr, scope, classes),
        Expr::Call { name: _, args } => {
            for arg in args {
                translate_expr(arg, scope, classes);
            }
        },
        Expr::Index { object, i } => {
            translate_node(&mut *object, scope, classes);
            translate_expr(i, scope, classes);
        },
        Expr::Get { left, right } => {
            translate_expr(left, scope, classes);
            translate_expr(right, scope, classes);
            translate_to_desired_form(node, scope);
        },
        Expr::MakePointer { expr, is_mut: _ } => translate_expr(&mut *expr, scope, classes),
        Expr::Deref { expr } => translate_expr(&mut *expr, scope, classes),
        Expr::This { callee } => translate_expr(&mut *callee, scope, classes),
        _ => (),
    }
}

fn translate_nodes(nodes: &mut Vec<AstNode>, scope: &mut Scope, classes: &Classes) {
    for node in nodes {
        translate_node(node, scope, classes);
    }
}

fn translate_node(node: &mut AstNode, scope: &mut Scope, classes: &Classes) {
    match node {
        AstNode::Expr(e) => translate_expr(e, scope, classes),
        AstNode::Stmt(s) => match s.obj_mut() {
            Stmt::Block { nodes } => {
                scope.add_scope();
                translate_nodes(nodes, scope, classes);
                for node in nodes {
                    translate_node(node, scope, classes);
                }
                scope.pop_scope();
            },
            Stmt::VarCreation { is_mut: _, name: _, ty: _, value: _ } => 
                create_var_if_obj(&s, classes, scope),
            Stmt::Assignment { left, right } => {
                for element in [left, right] {
                    translate_expr(element, scope, classes);
                }
            },
            Stmt::If { condition, true_branch, false_branch } => {
                translate_expr(condition, scope, classes);
                translate_node(&mut *true_branch, scope, classes);
                if let Some(branch) = false_branch {
                    translate_node(&mut *branch, scope, classes);
                }
            },
            Stmt::Return { val } => if let Some(val) = val {
                translate_expr(val, scope, classes)
            },
            Stmt::While { condition, body } => {
                translate_expr(condition, scope, classes);
                translate_node(&mut *body, scope, classes);
            },
            Stmt::Break | Stmt::Continue | Stmt::Poison => (),
            
        },
    }
}

fn try_to_construct_call(node: &Spanned<Expr>, scope: &Scope) -> Option<AstNode> {
    let e_span = node.just_span_data();
    match node.obj_ref() {
        Expr::Get { left, right } => {
            let Some(class) = try_to_get_var_as_class(&left, &scope) else { return None };
            match &right.obj {
                Expr::Call { name, args } => {
                    let mut new_args = vec![Spanned::zeroed(
                        Expr::This { callee: left.clone() }
                    )];
                    new_args.extend(args.to_vec().into_iter());
                    Some(AstNode::Expr(Spanned::new(Expr::Call { 
                        name: Spanned::from_other_span(
                            format!("{}.{}", class.name, name.to_string()),
                            &e_span
                        ), 
                        args: new_args
                    }, 0, 0)))
                },
                _ => None,
            }
        },
        _ => None,
    }
}

fn expr_type_to_class<'a>(ty: &'a ExprType, scope: &'a Scope) -> Option<&'a ParsedClass> {
    match ty {
        ExprType::Class(c) => Some(c),
        ExprType::Pointer { points_to, is_mut: _ } => 
            expr_type_to_class(points_to, scope),
        _ => None,
    }   
}

fn try_to_get_var_as_class<'a>(node: &Expr, scope: &'a Scope) -> Option<&'a ParsedClass> {
    let Some(var) = try_to_get_var(node, scope) else { return None };
    expr_type_to_class(&var.ty, scope)  
}

fn try_to_get_var<'a>(node: &Expr, scope: &'a Scope) -> Option<&'a Variable> {
    match node {
        Expr::Var(v) => match scope.get_var(&v) {
            Ok(v) => Some(v),
            Err(_) => None,
        },
        _ => None,
    }
}


fn create_var_if_obj(node: &Stmt, classes: &Classes, scope: &mut Scope) {
    match node {
        Stmt::VarCreation { is_mut, name, ty: _, value } => {
            let var_name = name;
            if let Some(value) = value {
                match value.obj_ref() {
                    Expr::Call { name, args: _ } => {
                        if let Some(class) = classes.get(&**name) {
                            let ty = class.get_return_type(value);
                            let var = Variable::new(ty, *is_mut, true);
                            scope.add_var(var_name, var);
                        }
                    },
                    Expr::MakePointer { expr, is_mut: _ } => {
                        match expr.obj_ref() {
                            Expr::Var(v) => if let Ok(var) = scope.get_var(&v) {
                                scope.add_var(var_name, Variable::new(ExprType::Pointer { 
                                    points_to: Box::new(var.ty.clone()), is_mut: false }, false, false))
                            },
                            _ => (),
                        }
                    }
                    _ => (),
                }
            }
        }
        _ => (),
    };
}
