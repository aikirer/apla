use super::{bytecode::OpCode, stack::{Stack, StackVal}, error::RTError, scope::Scope};
// use crate::binary_op;

#[derive(Debug)]
pub struct VM {
    stack: Stack,
    scope: Scope,
}

// prefixed with Op
use OpCode::*;

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            scope: Scope::new(),
        }
    }

    pub fn execute(&mut self, code: &[OpCode]) -> Result<(), RTError> {
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
                    }
                },
                OpElse(else_len) => {
                    // skip the whole block
                    at += else_len;
                },
            }
            at += 1;
        }
        println!("{:?}\n{:?}", self.scope, self.stack.pop());
        Ok(())
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
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

