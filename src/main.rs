#![allow(unused)]

use std::{collections::HashMap, io::Read};

use cranelift::codegen::ir::types::I64;
use lexer::lexer::Lexer;

use crate::{
    backend::cranelift::compile_program,
    frontend::variable::MsVariable,
    lexer::ast::Program,
    lexer::{
        ast::{FunctionDeclaration, FunctionSignature, Type},
        code_parser::{CodeParser, CodeWord},
    },
    libc::libc::malloc_test,
    utils::{rc_str::RcStr, rc_vec::RcVec},
};

mod backend;
mod frontend;
mod lexer;
mod libc;
mod utils;

fn main() {
    let bytes = malloc_test().unwrap();
    std::fs::write("/tmp/malloc.o", bytes).unwrap();
    println!("");
}

pub fn create_executable(input_file: &str, output_file: &str) -> anyhow::Result<()> {
    let child = std::process::Command::new("gcc")
        .arg(input_file)
        .arg("-o")
        .arg(output_file)
        .spawn()?;
    Ok(())
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
