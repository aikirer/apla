use apla_std::Std;
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
pub mod call;
pub mod class;
pub mod apla_std;
pub mod named_obj_container;

pub fn run(input: String) -> Result<(), ()> {
    let apla_std = Std::new();
    let mut scanner = Scanner::new(&input);
    let tokens = crate::measure_time!(scanner.scan(), "scanning");
    let parser = Parser::new(tokens, &input);
    let (mut ast, had_error, mut functions, classes) = {
        let comp = crate::measure_time!(parser.parse(), "parsing");
        (comp.ast, comp.had_error, comp.functions, comp.classes)
    };
    if had_error {
        return Err(())
    } 
    crate::measure_time!({
        ast.optimize();
        for code in functions.values_mut() {
            code.node.optimize();
        }
    }, "optimizing");
    // println!("ast after optimizing: {ast:?}");
    let callables = crate::measure_time!(
        match Resolver::resolve(
            &mut ast, &input, &functions, classes, apla_std
        ) 
        {
            Ok(functions) => functions,
            Err(_) => {
                return Err(())
            },
        }, "resolving"
    );
    let (bytecode, callables) = {
        let compiler = Compiler::new(&ast, callables);
        crate::measure_time!(compiler.compile(), "compiling")
    };
    println!("generated code: {bytecode:?}");
    let functions = callables.iter()
        .map(|(name, callable)| (name.to_string(), callable.as_ref()))
        .collect();
    let mut vm = VM::new(functions);
    if let Err(er) = crate::measure_time!(vm.execute(&bytecode), "executing") {
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
