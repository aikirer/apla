use std::collections::HashMap;

use compile_time::util::func::ParsedFunc;
use run_time::bytecode::OpCode;
use scanner::Scanner;

use crate::{
    compile_time::{parser::Parser, 
        ast_compiler::Compiler, optimize::Optimize, resolver::Resolver}, 
    run_time::vm::VM, call::Call
};

pub mod expr_type;
pub mod scanner;
pub mod compile_time;
pub mod token;
pub mod run_time;
pub mod spanned;
pub mod call;

pub fn compile(
    input: &str
) -> Result<(Vec<OpCode>, HashMap<String, ParsedFunc>), ()> 
{
    let mut scanner = Scanner::new(&input);
    let tokens = crate::measure_time!(scanner.scan(), "scanning");
    println!("result: {tokens:?}");
    let parser = Parser::new(tokens, &input);
    let (mut ast, had_error, mut functions) = {
        let comp = crate::measure_time!(parser.parse(), "parsing");
        (comp.ast, comp.had_error, comp.functions)
    };
    println!("ast: {ast:?}");
    if had_error {
        return Err(())
    } 
    crate::measure_time!({
        ast.optimize();
        for (_, code) in &mut functions {
            code.node.optimize();
        }
    }, "optimizing");
    let resolver = Resolver::new(&ast, input, &functions);
    let functions = crate::measure_time!(match resolver.resolve() {
        Ok(functions) => functions,
        Err(_) => {
            return Err(())
        },
    }, "resolving");
    println!("ast after optimizing: {ast:?}");
    let (bytecode, functions) = {
        let compiler = Compiler::new(&ast, functions);
        crate::measure_time!(compiler.compile(), "compiling")
    };
    Ok((bytecode, functions))
    
}

pub fn run(input: String) -> Result<(), ()> {
    let (bytecode, mut funcs) = compile(&input).unwrap_or_else(|_| std::process::exit(1));
    println!("generated code: {bytecode:?}");
    let functions: HashMap<String, &dyn Call> = funcs
        .iter_mut()
        .map(|(name, func)| (name.clone(), func as &dyn Call))
        .collect();
    let mut vm = VM::new(functions);
    if let Err(er) = vm.execute(&bytecode) {
        println!("[RUNTIME ERROR] {er}");
    }
    Ok(())
}

pub fn repl() {
    loop {
        let mut buf = String::new();
        if let Err(er) = std::io::stdin().read_line(&mut buf) {
            eprintln!("Couldn't read input! {er}");
            std::process::exit(1);
        };
        _ = run(buf);
    }
}

pub fn run_file(file_name: &str) {
    let content = match std::fs::read_to_string(file_name.trim()) {
        Ok(s) => s,
        Err(er) => {
            eprintln!("couldn't read the file '{file_name}': {er}");
            std::process::exit(1);
        }
    };
    _ = run(content);
}

#[macro_export]
macro_rules! measure_time {
    ($func: expr, $name: expr) => {{      
        let start = std::time::Instant::now();
        let r = $func;
        println!("{} took {}ms", $name, std::time::Instant::now()
                .duration_since(start)
                .as_millis());
        r
    }};
}
