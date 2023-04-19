use std::collections::HashMap;

use crate::compile_time::error::CTErrorKind;

use super::variable::Variable;

#[derive(Debug)]
pub struct Scope {
    scopes: Vec<SingleScope>
}

impl Scope {
    pub fn new() -> Self {
        Self {
            scopes: vec![SingleScope::new()]
        }
    }

    pub fn add_var(&mut self, name: &str, var: Variable) {
        self.get_current_scope_mut().add_var(name, var);
    }

    pub fn get_current_scope(&self) -> &SingleScope {
        self.scopes.last().unwrap()
    }

    pub fn get_current_scope_mut(&mut self) -> &mut SingleScope {
        self.scopes.last_mut().unwrap()
    }

    pub fn get_var(
        &self, name: &str
    ) -> Result<&Variable, CTErrorKind> 
    {
        match self.scopes.iter()
            .rev()
            .find_map(|v| { v.objects.iter()
                .find(|(var_name, _)| &**var_name == name) })
                .map(|(_, val)| val) {
                    Some(val) => Ok(val),
                    None => Err(CTErrorKind::VarDoesntExist(name.to_string())),
                }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct SingleScope {
    pub objects: HashMap<String, Variable>,
}

impl SingleScope {
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
        }
    }

    pub fn add_var(&mut self, name: &str, var: Variable) {
        self.objects.insert(name.to_string(), var);
    }
}


impl Default for SingleScope {
    fn default() -> Self {
        Self::new()
    }
}
