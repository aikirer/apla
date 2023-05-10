use std::{collections::HashMap, fmt::Debug, cell::RefCell};

use crate::{expr_type::ExprType, run_time::{stack::StackVal, error::RTError, vm::VM, bytecode::OpCode}, call::Call, spanned::Spanned, compile_time::{ast::expr::Expr, resolver::Resolver, error::{CTError, CTErrorKind}, compile::{self, Compile}}};

#[derive(Debug)]
pub struct Std {
    pub functions: HashMap<String, StdFunc>,
    pub latest_call: RefCell<String>,
}

impl Std {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        functions.insert("print".to_string(),
            StdFunc::new(&[ExprType::Any], Box::new(Self::print), ExprType::Null));
        functions.insert("rust_dbg_print".to_string(),
            StdFunc::new(&[ExprType::Any], Box::new(Self::rust_dbg_print), ExprType::Null));
        functions.insert("read".to_string(),
            StdFunc::new(&[], Box::new(Self::read), ExprType::String));
        functions.insert("strlen".to_string(),
            StdFunc::new(&[ExprType::String], Box::new(Self::strlen), ExprType::Int));
        functions.insert("str_to_int".to_string(),
            StdFunc::new(
                &[ExprType::String], 
                Box::new(Self::str_to_int), 
                ExprType::Int
            )
        );
        Self {  
            functions,
            latest_call: RefCell::new("".to_string()),
        }
    } 

    pub fn get_func(&self, name: &str) -> Result<&StdFunc, CTErrorKind> {
        match self.functions.get(name) {
            Some(func) => Ok(func),
            None => return Err(CTErrorKind::FuncDoesntExist(name.to_string())),
        }
    }

    fn print(args: &[StackVal]) -> StackVal {
        println!("{}", args[0]);
        StackVal::Null
    }

    fn rust_dbg_print(args: &[StackVal]) -> StackVal {
        println!("{:?}", args[0]);
        StackVal::Null
    }

    fn read(_args: &[StackVal]) -> StackVal {
        // TODO: these functions need to return a Result<StackVal, RTError>
        let mut buf = String::new();
        match std::io::stdin().read_line(&mut buf) {
            Ok(_) => (),
            Err(er) => {
                eprintln!("Couldn't read the input: {er}");
                std::process::exit(1);
            }
        };
        StackVal::String(buf.trim().to_string())
    }

    fn strlen(args: &[StackVal]) -> StackVal {
        let arg = match &args[0] {
            StackVal::String(s) => s,
            _ => panic!(),
        };
        StackVal::Int(arg.len() as i32)
    }

    fn str_to_int(args: &[StackVal]) -> StackVal {
        let arg = match &args[0] {
            StackVal::String(s) => s,
            _ => panic!(),
        };
        StackVal::Int(
            arg.parse().expect("Invalid input!")
        )
    }
}

pub type FuncContainer = Box<dyn Fn(&[StackVal]) -> StackVal>;

pub struct StdFunc {
    pub args: &'static [ExprType],
    pub return_type: ExprType,
    pub func: FuncContainer,
}

impl Debug for StdFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "std function taking {:?}", self.args)
    }
}

impl StdFunc {
    pub fn new(
        args: &'static [ExprType], func: FuncContainer, return_type: ExprType
    ) -> Self 
    {
        Self { args, func, return_type }
    }

    fn call(&self, args: &[StackVal]) -> StackVal {
        (*self.func)(args)
    }
}

impl Call for Std {
    fn get_arg_list(&self) -> Vec<&ExprType> {
        panic!()
    }

    fn compile_call(
        &self, args: &[Spanned<Expr>], ctx: &compile::Ctx
    ) -> Vec<OpCode> {
        let mut out: Vec<OpCode> = vec![];
        if args.len() != 1 { panic!(); }
        let call = &args[0];
        match call.obj_ref() {
            Expr::Call { name, args } => {
                for arg in args {
                    out.extend(arg.compile(ctx));
                }
                out.push(OpCode::OpString(name.to_string()));
                out.push(OpCode::OpCall("std".to_string()));
            },
            _ => panic!(),
        }
        out
    }

    fn resolve(
        &self, node: &Spanned<Expr>, resolver: &Resolver
    ) -> Result<(), Spanned<CTError>> {
        match node.obj_ref() {
            Expr::Call { name: _, args } => {
                if args.len() != 1 {
                    return Err(Spanned::from_other_span(
                        CTError::new(CTErrorKind::WrongArgCount(1)),
                         node));
                }
                match args[0].obj_ref() {
                    Expr::Call { name, args } => {
                        let func = match self.get_func(name.obj_ref()) {
                            Ok(func) => func,
                            Err(er) => return Err(
                                Spanned::from_other_span(CTError::new(er), name)
                                ),
                        };
                        self.latest_call.replace(name.to_string());
                        if func.args.len() != args.len() {
                            return Err(Spanned::from_other_span(
                                CTError::new(CTErrorKind::WrongArgCount(func.args.len())), 
                                node));
                        }
                        let got_ty = {
                            let mut out = vec![];
                            for arg in args {
                                out.push(Spanned::from_other_span(
                                    resolver.resolve_expr(arg)?, arg));
                            }
                            out
                        };
                        for (expected_ty, got_ty) in func.args.iter().zip(got_ty) {
                            if expected_ty == &ExprType::Any { continue; }
                            if expected_ty != got_ty.obj_ref() {
                                return Err(Spanned::from_other_span(CTError::new(
                                    CTErrorKind::MismatchedTypes(
                                        expected_ty.clone(), got_ty.cloned()
                                    )
                                ), &got_ty));
                            }
                        }
                        Ok(())
                    },
                    _ => Err(Spanned::from_other_span(
                        CTError::new(CTErrorKind::ExpectedFuncCall), 
                        node)
                    ),
                }
            }
            _ => panic!(),
        }
    }

    fn compile_to_code(&self, _ctx: &compile::Ctx) { }

    fn call(
        &self, vm: &mut VM
    ) -> Result<StackVal, RTError> {
        let func_name = match vm.stack.pop()? {
            StackVal::String(s) => s,
            other => return Err(RTError::MismatchedTypes(
                ExprType::String, other.to_expr_type()))
        };
        let func = match self.get_func(&func_name) {
            Ok(f) => f,
            Err(_) => return Err(RTError::ExpectedCallable),
        };
        let mut args = vec![];
        for _ in func.args {
            args.push(vm.stack.pop()?);
        }
        Ok(func.call(&args))
    }

    fn get_return_type(&self, node: &Spanned<Expr>) -> ExprType {
        match node.obj_ref() {
            Expr::Call { name: _, args } => {
                match &args[0].obj_ref() {
                    &Expr::Call { name, args: _ } => {
                        // You can unwrap here because you try to call
                        // the func in the resolver before
                        // calling this
                        self.get_func(&name).unwrap().return_type.clone()
                    },
                    _ => panic!(),
                }
            },
            _ => panic!(),
        }
    }
}
