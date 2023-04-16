use std::ops::{Add, Sub, Mul, Rem, Neg, Div};

use crate::expr_type::ExprType;

use super::error::RTError;

#[derive(Debug, Clone)]
pub enum StackVal {
    Int(i32), Float(f32), String(String),
}

impl StackVal {
    pub fn to_expr_type(&self) -> ExprType {
        match self {
            StackVal::Int(_) => ExprType::Int,
            StackVal::Float(_) => ExprType::Float,
            StackVal::String(_) => ExprType::String,
        }
    }
}

#[derive(Debug)]
pub struct Stack {
    values: Vec<StackVal>
}

impl Stack {
    pub fn new() -> Self {
        Self {
            values: vec![]
        }
    }

    pub fn push<T>(&mut self, val: T) 
    where
        StackVal: From<T>,
    {
        self.values.push(StackVal::from(val));
    }

    pub fn push_dir(&mut self, val: StackVal) {
        self.values.push(val);
    }

    pub fn pop(&mut self) -> Result<StackVal, RTError> {
        match self.values.pop() {
            Some(v) => Ok(v),
            None => Err(RTError::EmptyStack)
        }
    }

    // pub fn pop_with_val<T>(&mut self) -> Result<T, RTError> 
    // where 
    //     StackVal: From<T>,
    //     T: std::convert::TryFrom<super::stack::StackVal>
    // {
    //     match self.pop()? {
    //         Some(val) => if let Ok(val) = val.clone().try_into() {
    //             Some(val)
    //         } else {
    //             self.push_dir(val);
    //             None
    //         },
    //         None => None,
    //     }   
    // }

    // pub fn pop_with_val_twice<T>(&mut self,) -> Option<(T, T)> 
    // where 
    //     StackVal: From<T>,
    //     T: std::convert::TryFrom<super::stack::StackVal>
    // {
    //     match self.pop_with_val::<T>() {
    //         Some(val1) => match self.pop_with_val() {
    //             Some(val2) => Some((val2, val1)),
    //             None => {
    //                 self.push(val1);
    //                 None
    //             }
    //         },
    //         None => None
    //     }
    // }
}

impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! impl_from_for_stack_val {
    ($type: ty, $ex_val: expr, $variant: ident) => {
        impl From<$type> for StackVal {
            fn from(value: $type) -> Self {
                Self::$variant(value)
            }
        }
        
        impl TryFrom<StackVal> for $type {
            type Error = RTError;
        
            fn try_from(value: StackVal) -> Result<Self, Self::Error> {
                match value {
                    StackVal::$variant(v) => Ok(v),
                    _ => Err(RTError::MismatchedTypes(StackVal::$variant($ex_val).to_expr_type(), value.to_expr_type())),
                }
            }
        }
    };
}

impl_from_for_stack_val!(i32, 0, Int);
impl_from_for_stack_val!(f32, 0.0, Float);
impl_from_for_stack_val!(String, "".to_string(), String);

impl Add for StackVal {
    type Output = Result<StackVal, RTError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (&self, &rhs) {
            (StackVal::Int(a), StackVal::Int(b)) => Ok(StackVal::Int(a + b)),
            (StackVal::Float(a), StackVal::Float(b)) => Ok(StackVal::Float(a + b)),
            (StackVal::String(a), StackVal::String(b)) => Ok(StackVal::String(format!("{a}{b}"))),
            _ => Err(RTError::MismatchedTypes(self.to_expr_type(), rhs.to_expr_type())),
        }
    }
}

impl Neg for StackVal {
    type Output = Result<StackVal, RTError>;

    fn neg(self) -> Self::Output {
        match self {
            StackVal::Int(a) => Ok(StackVal::Int(-a)),
            StackVal::Float(a) => Ok(StackVal::Float(-a)),
            _ => Err(RTError::WrongType(self.to_expr_type())),
        }
    }
}

macro_rules! impl_binary {
    ($($trait_name: ident, $fn_name: ident, $op: tt)+) => {
        $(
            impl $trait_name for StackVal {
                type Output = Result<StackVal, RTError>;
            
                fn $fn_name(self, rhs: Self) -> Self::Output {
                    match (&self, &rhs) {
                        (StackVal::Int(a), StackVal::Int(b)) => Ok(StackVal::Int(a $op b)),
                        (StackVal::Float(a), StackVal::Float(b)) => Ok(StackVal::Float(a $op b)),
                        _ => Err(RTError::MismatchedTypes(self.to_expr_type(), rhs.to_expr_type())),
                    }
                }
            }
        )*
        
    };
}

impl_binary!(
    Sub, sub, -
    Mul, mul, *
    Div, div, /
    Rem, rem, %
);
