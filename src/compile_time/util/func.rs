use std::{collections::HashMap, cell::RefCell};

use crate::{
    expr_type::ExprType, 
    compile_time::{ast::{AstNode, expr::Expr}, compile::{Compile, self}, resolver::Resolver, error::{CTError, CTErrorKind}}, 
    spanned::Spanned, 
    run_time::{
        bytecode::OpCode, 
        vm::VM, 
        stack::StackVal, 
        error::RTError}, 
    call::Call
};

use super::variable::Variable;

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub return_type: Spanned<String>,
    pub args: Vec<AstNode>,
    pub node: AstNode,
}

impl Func {
    pub fn new(
        name: String,
        return_type: Spanned<String>, args: Vec<AstNode>,
        node: AstNode,
    ) -> Self {
        Self {
            name, return_type, args, node
        }
    }
}

#[derive(Debug)]
pub struct ParsedFunc {
    pub name: String,
    pub return_type: ExprType,
    pub args: HashMap<String, Variable>,
    pub code: RefCell<Vec<OpCode>>,
}


impl ParsedFunc {
    pub fn new(
        name: String,
        return_type: ExprType, args: HashMap<String, Variable>,
    ) -> Self {
        Self {
            name,
            return_type, args, 
            code: RefCell::new(vec![])
        }
    }
}

impl Call for ParsedFunc {
    fn get_arg_list(&self) -> Vec<&ExprType> {
        self.args.iter().map(|e| &e.1.ty).collect()
    }

    fn resolve(
        &self, node: &Spanned<Expr>, resolver: &Resolver
    ) -> Result<(), Spanned<CTError>> {
        let arg_list: &[&ExprType] = &self.get_arg_list();
        match node.obj_ref() {
            Expr::Call { name: _, args } => {
                let got_arg_types = args.iter()
                    .map(|e| resolver.resolve_expr(e))
                    .collect::<Vec<_>>();
                let wanted_len = arg_list.len();
                if got_arg_types.len() != wanted_len {
                    return Err(
                        Spanned::from_other_span(CTError::new(
                        CTErrorKind::WrongArgCount(wanted_len)
                            ), node)
                        );
                }
                for (i, (got_type, expected_type)) in 
                    got_arg_types.iter()
                        .zip(arg_list) 
                        .enumerate()
                {
                    match got_type {
                        Ok(t) => if t != *expected_type {
                            return Err(
                                Spanned::from_other_span(CTError::new(
                                    CTErrorKind::MismatchedTypes(
                                        (*expected_type).clone(), t.clone()
                                    )
                                ), dbg!(&args[i]))
                            )
                        },
                        Err(er) => return Err(er.clone()),
                    }
                }
            }
            _ => panic!(),
        };
        Ok(())
    }

    fn compile_call(
        &self, nodes: &[Spanned<Expr>], ctx: &compile::Ctx
    ) -> Vec<OpCode> 
    {
        
        let mut out = vec![OpCode::OpPushScope];
        for (e, name) in nodes.iter()
                .zip(self.args.iter().map(|e| e.0)) 
        {
            out.extend(e.compile(ctx));
            out.push(OpCode::OpCreateVar(name.to_string()));
            out.push(OpCode::OpGetVar(name.to_string()));
            out.push(OpCode::OpSet);
        }
        out.push(OpCode::OpCall(self.name.to_string()));
        out.push(OpCode::OpPopScope);
        out
    }

    fn call(&self, vm: &mut VM) -> Result<StackVal, RTError> {
        let intitial_scope_amount = vm.scope.objects.len();
        let result = vm.execute(&self.code.borrow());
        while vm.scope.objects.len() > intitial_scope_amount {
            vm.scope.pop_scope();
        };
        result
    }

    fn as_parsed_func(&self) -> Option<&ParsedFunc> {
        Some(self)
    }
}



