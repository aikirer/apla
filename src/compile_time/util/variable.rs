use crate::expr_type::ExprType;

#[derive(Debug)]
pub struct Variable {
    ty: ExprType,
    is_mut: bool,
    poisoned: bool,
    initialized: bool,
}

impl Variable {
    pub fn new(ty: ExprType, is_mut: bool, initialized: bool) -> Self {
        Self {
            ty, is_mut, initialized,
            poisoned: false,
        }
    }

    pub fn poison(&mut self) {
        self.poisoned = true;
    }

    pub fn is_poisoned(&self) -> bool {
        self.poisoned
    }
}
