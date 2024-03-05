#![allow(unused)]

use std::io::Read;

use lexer::lexer::Lexer;

use crate::{
    backend::cranelift::compile_program,
    lexer::ast::Program,
    lexer::{
        ast::{FunctionDeclaration, FunctionSignature, Type},
        code_parser::{CodeParser, CodeWord},
    },
    utils::{rc_str::RcStr, rc_vec::RcVec},
};

mod backend;
mod lexer;
mod utils;

fn main() {
    // let file_path = std::env::args().skip(1).next().unwrap();
    // compile_file(&file_path);
    let type_i32 = Type::new("i32");
    let type_void = Type::new("void");

    let print_fn = FunctionDeclaration::new(
        FunctionSignature::new("print", vec![type_i32.clone()], type_void.clone()),
        vec![],
    );

    let main_fn = FunctionDeclaration::new(
        FunctionSignature::new("main", vec![], type_void.clone()),
        vec![],
    );

    let program = Program::new(
        vec![type_i32.clone(), type_void.clone()],
        vec![print_fn, main_fn],
    );

    compile_program(program, "/tmp/mantis_out.o").unwrap();

    println!("");
}

pub fn compile_file(file_path: &str) {
    let mut s = String::new();
    let mut file = std::fs::OpenOptions::new()
        .read(true)
        .open(file_path)
        .unwrap();
    file.read_to_string(&mut s);

    let code_parser = CodeParser::new(s.into());
    let code_words = code_parser.parse().unwrap();

    code_words.iter().for_each(|x| {
        match x {
            CodeWord::Symbol(bp) => print!("Symbol: {:x?}\n", bp.as_str()),
            CodeWord::Word(word) => print!("WORD: '{word}'\n"),
        };
    });
}
