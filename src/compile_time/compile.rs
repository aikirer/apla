use std::collections::HashMap;

use crate::{run_time::bytecode::OpCode, call::Call};

pub type Output = Vec<OpCode>;

#[derive(Debug)]
pub struct Ctx<'a> {
    pub callables: &'a HashMap<String, Box<dyn Call>>
}

impl<'a> Ctx<'a> {
    pub fn get(&self, name: &str) -> &dyn Call {
        if name.contains('.') {        
            let parts = name.split('.').collect::<Vec<_>>();
            if parts.len() != 2 { panic!() }
            let (class_name, method) = (parts[0], parts[1]);
            self.callables.get(class_name).unwrap().as_obj().unwrap()
                .methods
                .get(method)
                .map(|method| method as &dyn Call)
                .unwrap()
        } else {
            self.callables.get(name).unwrap().as_ref()
        }

    }
}

pub trait Compile {
    fn compile(&self, ctx: &Ctx) -> Output;    

    fn add(&self, out: &mut Output, instr: OpCode) {
        out.push(instr);
    }
}

// implementations in ast_compiler.rs
