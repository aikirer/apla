use run_time::bytecode::OpCode;
use scanner::Scanner;

use crate::{
    compile_time::{parser::Parser, 
        ast_compiler::Compiler, optimize::Optimize, resolver::Resolver}, 
    run_time::vm::VM
};

pub mod expr_type;
pub mod scanner;
pub mod compile_time;
pub mod token;
pub mod run_time;
pub mod spanned;

pub fn compile(input: &str) -> Result<Vec<OpCode>, ()> {
    let mut scanner = Scanner::new(&input);
    let tokens = crate::measure_time!(scanner.scan(), "scanning");
    println!("result: {tokens:?}");
    let parser = Parser::new(tokens, &input);
    let (mut ast, had_error) = crate::measure_time!(match parser.parse() {
        Some(ast) => ast,
        None => {
            eprintln!("Cannot compile an empty program!");
            return Ok(vec![]);
        }
    }, "parsing");
    println!("ast: {ast:?}");
    if had_error {
        return Err(())
    } 
    crate::measure_time!(ast.optimize(), "optimizing");
    let mut resolver = Resolver::new(&ast, input);
    crate::measure_time!(match resolver.resolve() {
        Ok(()) => (),
        Err(_) => {
            return Err(())
        },
    }, "resolving");
    println!("ast after optimizing: {ast:?}");
    let mut compiler = Compiler::new(&ast);
    let bytecode = crate::measure_time!(compiler.compile(), "compiling");
    Ok(bytecode)
}

pub fn run(input: String) -> Result<(), ()> {
    let bytecode = compile(&input).unwrap_or_else(|_| std::process::exit(1));
    println!("generated code: {bytecode:?}");
    let mut vm = VM::new();
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
