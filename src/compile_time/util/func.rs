use std::collections::HashMap;

use crate::{
    expr_type::ExprType, 
    compile_time::{ast::AstNode, compile::Compile}, 
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
    pub return_type: Spanned<String>,
    pub args: Vec<AstNode>,
    pub node: AstNode,
}

impl Func {
    pub fn new(
        return_type: Spanned<String>, args: Vec<AstNode>,
        node: AstNode,
    ) -> Self {
        Self {
            return_type, args, node
        }
    }
}

#[derive(Debug)]
pub struct ParsedFunc {
    pub return_type: ExprType,
    pub args: HashMap<String, Variable>,
    pub code: Vec<OpCode>,
}


impl ParsedFunc {
    pub fn new(
        return_type: ExprType, args: HashMap<String, Variable>,
    ) -> Self {
        Self {
            return_type, args, 
            code: vec![]
        }
    }
}

impl Call for ParsedFunc {
    fn get_arg_list(&self) -> Vec<&ExprType> {
        self.args.iter().map(|e| &e.1.ty).collect()
    }

    fn compile_call(&self, node: &AstNode) -> Vec<OpCode> {
        node.compile()
    }

    fn runtime_call(&self, vm: &mut VM) -> Result<StackVal, RTError> {
        vm.execute(&self.code)
    }
}
