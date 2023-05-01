use std::{collections::HashMap, rc::Rc};

use crate::call::Call;

use super::{
    bytecode::OpCode, 
    stack::{Stack, StackVal}, 
    error::RTError, 
    scope::Scope
};

#[derive(Debug)]
pub struct VM<'a> {
    pub stack: Stack,
    pub scope: Scope,
    pub callables: HashMap<String, &'a dyn Call>
}

// prefixed with Op
use OpCode::*;

macro_rules! skip_until {
    ($until: pat, $code: expr, $at: expr) => {
        loop {
            $at += 1;
            let op = match $code.get($at) {
                Some(o) => o,
                None => panic!(),
            };
            if let $until = op {
                break;
            }
        }
    };
}

impl<'a> VM<'a> {
    pub fn new(
        callables: HashMap<String, &'a dyn Call>
    ) -> Self 
    {
        Self {
            stack: Stack::new(),
            scope: Scope::new(),
            callables
        }
    }

    pub fn execute(&mut self, code: &[OpCode]) -> Result<StackVal, RTError> {
        let mut at = 0;
        while let Some(opcode) = code.get(at) {
            match opcode {
                op @ (OpAdd | OpMultiply | OpDivide |
                    OpModulo | OpSubtract | OpGreater |
                    OpSmaller | OpSmallerEqual | OpGreaterEqual | 
                    OpEqual | OpNotEqual) => self.bin_op(op)?,
                OpNegate => {
                    let a = self.stack.pop()?;
                    self.stack.push((-a)?);
                },
                OpNumber(n) => self.stack.push(*n),
                OpFloat(f) => self.stack.push(*f),
                OpString(s) => self.stack.push(s.to_string()),
                OpBool(b) => self.stack.push(*b),

                OpGetVar(n) => self.stack.push(self.scope.get_var(n)?),
                OpSet => {
                    let a = self.stack.pop_place()?;
                    let b = self.stack.pop()?;
                    a.replace(b);
                },
                OpCreateVar(n) => self.scope.add_var(n, StackVal::Null),
                OpPushScope => self.scope.push_scope(),
                OpPopScope => self.scope.pop_scope(),

                OpIf(if_len) => {
                    if !self.stack.pop()?.is_true()? {
                        // skip ahead of the else instr
                        at += if_len + 1;
                        // avoid the additional increment at the end of the
                        // loop
                        continue;
                    }
                },
                OpElse(else_len) => {
                    // skip the whole block
                    at += else_len;
                },

                OpCall(name) => {
                    match self.get_callable(name)?.call(self)? {
                        StackVal::Null => (),
                        other => self.stack.push(other),
                    };
                },  

                OpLoop(loop_len) => {
                    if !self.stack.pop()?.is_true()? {
                        at += loop_len + 1;
                    }
                },
                OpEndLoop(loop_len) => {
                    // If the condition of the loop is false
                    // the program skips AHEAD of this instruction,
                    // so this instruction always meets to head back
                    at -= loop_len;
                    // Continue to avoid adding to "at" at the end of the loop
                    continue; 
                },
                OpBreak => skip_until!(OpEndLoop(_), code, at),
                OpContinue => {
                    skip_until!(OpEndLoop(_), code, at);
                    // avoid one more increment to at which would break out of the
                    // loop
                    //println!("[LOG]: Hit continue");
                    continue;
                },

                OpIndex => todo!(),
                OpGetField(name) => {
                    let obj = self.stack.pop_object()?;
                    self.stack.push(Rc::clone(
                        obj.borrow().runtime_fields.get(name).unwrap()
                    ));
                }
                OpGetMethod(_name) => {
                    // self.stack.pop_object()?.borrow().methods
                    //     .get(&name.to_string())
                    //     .unwrap()
                    //     .call(self)?;
                },
                OpMakePointer => {
                    let pop = self.stack.pop_as_place()?;
                    self.stack.push_dir(StackVal::Pointer(Rc::new(pop)));
                    
                },
                OpDeref => {
                    let ptr = self.stack.pop_derefed_ptr()?;
                    self.stack.push_dir(StackVal::Var(ptr));
                }
                OpReturn => return self.stack.pop(),
            }
            at += 1;
        }
        Ok(StackVal::Null)
    }

    fn bin_op(&mut self, kind: &OpCode) -> Result<(), RTError>{
        let b = self.stack.pop()?;
        let a = self.stack.pop()?;
        self.stack.push_dir(match kind {
            OpAdd => (a + b)?,
            OpSubtract => (a - b)?,
            OpMultiply => (a * b)?,
            OpDivide => (a / b)?,
            OpModulo => (a % b)?,
            OpEqual => StackVal::Bool(a == b),
            OpNotEqual => StackVal::Bool(a != b),
            OpGreater => StackVal::Bool(a > b),
            OpGreaterEqual => StackVal::Bool(a >= b),
            OpSmaller => StackVal::Bool(a < b),
            OpSmallerEqual => StackVal::Bool(a <= b),
            _ => panic!()
        });
        Ok(())
    }

    fn get_callable(
        &self, name: &str
    ) -> Result<&'a dyn Call, RTError> 
    {
        if name.contains('.') {
            let (class, method) = {
                let parts: Vec<_> = name.split('.').collect();
                (parts[0], parts[1])
            };
            match self.get_callable(class)?
                .as_class()
                .unwrap()
                .methods
                .get(method)
                .map(|func| func as &dyn Call) 
            {
                Some(a) => Ok(a),
                None => Err(RTError::ExpectedCallable)
            }
        } else {        
            match self.callables.get(name) {
                Some(a) => Ok(a.clone()), // this clones a reference
                None => Err(RTError::ExpectedCallable)
            }
        }

    }
}

