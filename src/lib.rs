use scanner::Scanner;

use crate::{
    compile_time::{parser::Parser, 
        ast_compiler::Compiler, optimize::Optimize}, 
    run_time::vm::VM
};

pub mod expr_type;
pub mod scanner;
pub mod compile_time;
pub mod token;
pub mod run_time;
pub mod spanned;

pub fn run(input: String) {
    let mut scanner = Scanner::new(&input);
    let tokens = crate::measure_time!(scanner.scan(), "scanning");
    println!("result: {tokens:?}");
    let parser = Parser::new(tokens, &input);
    let mut ast = crate::measure_time!(parser.parse(), "parsing");
    println!("ast: {ast:?}");
    crate::measure_time!(ast.optimize(), "optimizing");
    println!("ast after optimizing: {ast:?}");
    let mut compiler = Compiler::new(&ast);
    let bytecode = crate::measure_time!(compiler.compile(), "compiling");
    println!("generated code: {bytecode:?}");
    let mut vm = VM::new();
    if let Err(er) = vm.execute(&bytecode) {
        println!("[ERROR] {er}");
    }
}

pub fn repl() {
    let mut buf = String::new();
    if let Err(er) = std::io::stdin().read_line(&mut buf) {
        eprintln!("Couldn't read input! {er}");
        std::process::exit(1);
    };
    run(buf);
}

pub fn run_file(_file_name: &str) {

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