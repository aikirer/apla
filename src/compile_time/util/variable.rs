use crate::expr_type::ExprType;

#[derive(Debug)]
pub struct Variable {
    ty: ExprType,
    is_mut: bool,
    poisoned: bool,
}

impl Variable {
    pub fn new(ty: ExprType, is_mut: bool) -> Self {
        Self {
            ty, is_mut, 
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
