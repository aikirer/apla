use std::collections::HashMap;

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
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct SingleScope {
    objects: HashMap<String, Variable>,
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
