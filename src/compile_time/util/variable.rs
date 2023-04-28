use std::cell::RefCell;

use crate::expr_type::ExprType;

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub ty: ExprType,
    pub is_mut: bool,
    pub poisoned: bool,
    pub initialized: RefCell<bool>,
}

impl Variable {
    pub fn new(ty: ExprType, is_mut: bool, initialized: bool) -> Self {
        Self {
            ty, is_mut, 
            initialized: RefCell::new(initialized),
            poisoned: false,
        }
    }

    pub fn poison(&mut self) {
        self.poisoned = true;
    }

    pub fn is_poisoned(&self) -> bool {
        self.poisoned
    }

    pub fn init(&self) {
        self.initialized.replace(true);
    }

    pub fn is_init(&self) -> bool {
        *self.initialized.borrow()
    }
}
