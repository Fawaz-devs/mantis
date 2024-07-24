#![allow(unused)]

use std::{collections::HashMap, io::Read};

use clap::Parser;
use cranelift::codegen::ir::types::I64;
use frontend::tokenizer::collect_to_tokens;
use lexer::lexer::Lexer;

use crate::{
    backend::cranelift::compile_program,
    frontend::{compiler, tokenizer::read_to_tokens, variable::MsVariable},
    lexer::{
        ast::{FunctionDeclaration, FunctionSignature, Program, Type},
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

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    // Input Mantis File
    input: String,

    // Print AST to a file or console
    #[arg(long)]
    ast: Option<String>,

    // Output executable
    #[arg(long, short)]
    output: Option<String>,
}

fn main() {
    env_logger::init();

    let args = Args::parse();

    let filepath = args.input;

    let input = std::fs::read_to_string(filepath).unwrap();

    // collect_to_tokens("let a = 100.2; b = 0.89");

    let fns = read_to_tokens(input);

    if let Some(ast_path) = args.ast {
        std::fs::write(ast_path, format!("{:#?}", fns)).unwrap();
    }

    if let Some(output_file_path) = args.output {
        create_executable(fns, &output_file_path);
    }
}

fn create_executable(fns: Vec<frontend::tokens::FunctionDeclaration>, output: &str) {
    let bytes = compiler::compile(fns).unwrap();
    let object_file = "/tmp/main.o";
    std::fs::write(object_file, bytes).unwrap();

    let mut child = std::process::Command::new("gcc")
        .arg(object_file)
        .arg("-o")
        .arg(output)
        .spawn()
        .unwrap();
    let exit_code = child.wait().unwrap();
    log::info!("gcc exit code: {}", exit_code);
    log::info!("compiled to {}", output);
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
