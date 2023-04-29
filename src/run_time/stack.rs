use std::{ops::{Add, Sub, Mul, Rem, Neg, Div}, rc::Rc, cell::RefCell, fmt::Display};

use crate::{expr_type::ExprType, class::Object};

use super::error::RTError;

#[derive(Debug, Clone, PartialEq)]
pub enum StackVal {
    Int(i32), Float(f32), String(String), Bool(bool),
    Var(Rc<RefCell<StackVal>>), Slice(Box<StackVal>), Null,
    Obj(Object),
    Object(Rc<RefCell<Object>>), Pointer(Rc<Rc<RefCell<StackVal>>>),
}

impl PartialOrd for StackVal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (StackVal::Int(a), StackVal::Int(b)) => Some(a.cmp(b)),
            (StackVal::Float(a), StackVal::Float(b)) => Some(a.total_cmp(b)),
            _ => None,
        }
    }
}

impl StackVal {
    pub fn to_expr_type(&self) -> ExprType {
        match self {
            StackVal::Int(_) => ExprType::Int,
            StackVal::Float(_) => ExprType::Float,
            StackVal::String(_) => ExprType::String,
            StackVal::Bool(_) => ExprType::Bool,
            StackVal::Var(v) => v.as_ref().borrow().to_expr_type(),
            StackVal::Slice(v) => v.to_expr_type(),
            StackVal::Object(_) => todo!(),
            StackVal::Obj(_) => todo!(),
            StackVal::Pointer(_) => todo!(),
            StackVal::Null => ExprType::ToBeInferred,
        }
    }

    pub fn to_stack_val(&self) -> StackVal {
        match self {
            StackVal::Var(v) => v.as_ref().borrow().to_stack_val(),
            t => t.clone(),
        }
    }

    pub fn is_true(&self) -> Result<bool, RTError> {
        match self {
            Self::Bool(b) => Ok(*b),
            _ => Err(RTError::ExpectedBool),
        }
    }

    pub fn derefed(&self) -> Result<Rc<RefCell<StackVal>>, RTError> {
        match self {
            StackVal::Var(v) => v.borrow().derefed(),
            StackVal::Pointer(s) => Ok(s.as_ref().clone()),
            _ => Err(RTError::ExpectedPtr),
        }
    }

    // pub fn get(&self, i: StackVal) -> StackVal {
    //     let i = match i {
    //         StackVal::Int(i) => i,
    //         _ => panic!(),
    //     };
    //     let i = match TryInto::<usize>::try_into(i) {
    //         Ok(a) => a,
    //         Err(_) => panic!("Can't index with a value of {i}!"),
    //     };
    //     match self {
    //         StackVal::String(s) => StackVal::Slice(Box::new(StackVal::String(
    //             s.get(i..=i).unwrap().to_string()
    //         ))),
    //         StackVal::Var(v) => StackVal::Slice(Box::new(StackVal::Var(
    //             Rc::new(RefCell::new(v.borrow())),
    //         ))),
    //         StackVal::Slice(s) => todo!(),
    //         _ => panic!(),
    //     }
    // }
}

impl Display for StackVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            StackVal::Int(i) => i.to_string(),
            StackVal::Float(f) => f.to_string(),
            StackVal::String(s) => s.to_string(),
            StackVal::Bool(b) => b.to_string(),
            StackVal::Var(v) => v.as_ref().borrow().to_string(),
            StackVal::Slice(s) => s.to_string(),
            StackVal::Object(_) => "object".to_string(),
            StackVal::Obj(_) => "object".to_string(),
            StackVal::Null => "void".to_string(),
            StackVal::Pointer(v) => format!("pointer to {}", v
                .as_ref().borrow()),
        })
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
        T: std::fmt::Debug
    {
        self.values.push(StackVal::from(val));
    }

    pub fn push_dir(&mut self, val: StackVal) {
        self.values.push(val);
    }

    fn normal_pop(&mut self) -> Result<StackVal, RTError> {
        match self.values.pop() {
            Some(v) => Ok(v),
            None => Err(RTError::EmptyStack)
        }
    }

    pub fn pop(&mut self) -> Result<StackVal, RTError> {
        match self.normal_pop() {
            Ok(v) => Ok(v.to_stack_val()),
            Err(er) => Err(er)
        }
    }

    pub fn pop_place(&mut self) -> Result<Rc<RefCell<StackVal>>, RTError> {
        let pop = self.normal_pop()?;
        match pop {
            StackVal::Var(v) => Ok(v),
            _ => Err(RTError::ExpectedPlace)
        }
    }

    pub fn pop_object(&mut self) -> Result<Rc<RefCell<Object>>, RTError> {
        match self.normal_pop()? {
            StackVal::Var(o) => match &*o.as_ref().borrow() {
                StackVal::Object(o) => Ok(Rc::clone(o)),
                _ => panic!(),
            }
            StackVal::Object(o) => Ok(o),
            _ => panic!(),
        }
    }

    pub fn pop_as_place(&mut self) -> Result<Rc<RefCell<StackVal>>, RTError> {
        Ok(match self.normal_pop()? {
            StackVal::Var(v) => v,
            StackVal::Obj(_) => todo!(),
            StackVal::Object(_) => todo!(),
            StackVal::Slice(_) => todo!(),
            other => Rc::new(RefCell::new(other)),
        })
    }

    pub fn pop_derefed_ptr(&mut self) -> Result<Rc<RefCell<StackVal>>, RTError> {
        self.normal_pop()?.derefed()
    }
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
impl_from_for_stack_val!(bool, false, Bool);

impl From<Rc<RefCell<StackVal>>> for StackVal {
    fn from(value: Rc<RefCell<StackVal>>) -> Self {
        Self::Var(value)
    }
}

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
