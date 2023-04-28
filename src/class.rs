use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::{call::Call, compile_time::{ast::{expr::Expr, AstNode}, compile, error::CTError, resolver::Resolver, util::{func::{ParsedFunc, Func}, variable::Variable}}, run_time::{bytecode::OpCode, stack::StackVal, error::RTError, vm::VM}, spanned::Spanned, expr_type::ExprType};

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub fields: Vec<AstNode>,
    pub methods: HashMap<String, Func>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            fields: Vec::new(),
            methods: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedClass {
    pub name: String,
    pub fields: HashMap<String, Variable>,
    pub methods: HashMap<String, ParsedFunc>,
}

impl ParsedClass {
    pub fn new(name: String) -> Self {
        Self {
            name,
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    pub runtime_fields: HashMap<String, Rc<RefCell<StackVal>>>,
    pub fields: HashMap<String, Variable>,
    pub methods: HashMap<String, ParsedFunc>, 
}

impl Object {
    pub fn new(
        fields: HashMap<String, Variable>, 
        methods: HashMap<String, ParsedFunc>
    ) -> Self 
    {
        Self {
            runtime_fields: HashMap::new(),
            fields, methods,
        }
    }
}

impl Call for ParsedClass {
    fn get_arg_list(&self) -> Vec<&crate::expr_type::ExprType> {
        vec![]
    }

    fn compile_call(
        &self, _args: &[Spanned<Expr>], _ctx: &compile::Ctx
    ) -> Vec<OpCode> {
        vec![OpCode::OpCall(self.name.to_string())]
    }

    fn resolve(
        &self, _node: &Spanned<Expr>, _resolver: &Resolver
    ) -> Result<(), Spanned<CTError>> {
        Ok(())
    }

    fn get_return_type(&self, _node: &Spanned<Expr>) -> ExprType {
        // TODO: this cannot just clone itself like this
        ExprType::Class(self.clone())
    }

    fn compile_to_code(&self, ctx: &compile::Ctx) {
        for method in self.methods.values() {
            method.compile_to_code(ctx);
        }
    }

    fn call(&self, _vm: &mut VM<'_>) -> Result<StackVal, RTError> {
        // TODO: fix cloning
        let mut object = Object::new(self.fields.clone(), self.methods.clone());
        object.runtime_fields = object.fields.iter()
            .map(|(name, _)| (name.to_string(), Rc::new(RefCell::new(StackVal::Null))))
            .collect();
        Ok(StackVal::Object(Rc::new(RefCell::new(object))))
    }

    fn as_obj(&self) -> Option<&ParsedClass> {
        Some(self)
    }
}
