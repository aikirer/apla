use std::{collections::HashMap, rc::Rc, cell::RefCell};

use super::{stack::StackVal, error::RTError};

pub type SingleScope = HashMap<String, Rc<RefCell<StackVal>>>;

#[derive(Debug)]
pub struct Scope {
    pub objects: Vec<SingleScope>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            objects: vec![HashMap::new()]
        }
    }

    pub fn add_var(&mut self, name: &str, val: StackVal) {
        self.get_current_scope().insert(name.to_string(), 
            Rc::new(RefCell::new(val))
        );
    }

    pub fn get_var(
        &self, name: &str
    ) -> Result<Rc<RefCell<StackVal>>, RTError> 
    {
        match self.objects.iter()
            .rev()
            .find_map(|v| { v.iter()
                .find(|(var_name, _)| **var_name == name) })
                .map(|(_, val)| Rc::clone(val)) {
                    Some(val) => Ok(val),
                    None => Err(RTError::VarDoesntExist(name.to_string())),
                }
    }

    pub fn push_scope(&mut self) {
        self.objects.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        dbg!(self.objects.pop());
    }

    fn get_current_scope(&mut self) -> &mut SingleScope {
        self.objects.last_mut().unwrap()
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new()
    }
}
