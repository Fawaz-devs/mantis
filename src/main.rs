#![allow(unused)]

use std::{collections::HashMap, io::Read};

use clap::Parser;
use cranelift::codegen::ir::types::I64;
use frontend::tokenizer::{collect_functions, collect_to_tokens};
use lexer::lexer::Lexer;

use crate::{
    backend::cranelift::compile_program,
    frontend::{
        compiler,
        tokenizer::read_to_tokens,
        tokens::{BuiltInType, StructMapBuilder, StructRegistry, VariableType},
        variable::MsVariable,
    },
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
pub mod registries;
pub mod scope;
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

// fn test() {
//     let mut registry = StructRegistry::new();
//     let mut s1 = StructMapBuilder::new();
//     s1.add_field("a", VariableType::BuiltIn(BuiltInType::I32), &registry);
//     s1.add_field("b", VariableType::BuiltIn(BuiltInType::I64), &registry);
//     s1.add_field("c", VariableType::BuiltIn(BuiltInType::F32), &registry);
//     dbg!(&s1);
//     registry.add_struct("s1".into(), s1);

//     let mut s2 = StructMapBuilder::new();
//     s2.add_field("d", VariableType::Custom("s1".into()), &registry);
//     s2.add_field("d2", VariableType::Custom("s1".into()), &registry);
//     s2.add_field("a", VariableType::BuiltIn(BuiltInType::I32), &registry);
//     // s2.add_field("b", VariableType::BuiltIn(BuiltInType::I64), &registry);
//     // s2.add_field("c", VariableType::BuiltIn(BuiltInType::F32), &registry);
//     dbg!(&s2);
// }

fn main() {
    // test();
    // panic!("Test is done");

    // env_logger::init();
    init_logger();

    let args = Args::parse();

    let filepath = args.input;

    let input = std::fs::read_to_string(filepath).unwrap();

    // collect_to_tokens("let a = 100.2; b = 0.89");

    let (fns, sr) = collect_functions(input);

    if let Some(ast_path) = args.ast {
        std::fs::write(ast_path, format!("{:#?}\n{:#?}", sr, fns)).unwrap();
    } else {
        dbg!(fns);
        dbg!(sr);
    }

    // if let Some(output_file_path) = args.output {
    //     create_executable(fns, sr, &output_file_path);
    // }
}

fn init_logger() {
    use std::io::Write;
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .format(|buf, record| {
            let ts = buf.timestamp();
            writeln!(
                buf,
                "{} [{}:{}] - {}",
                ts,
                record.file().unwrap_or("unknown"),
                record.line().unwrap_or(0),
                record.args()
            )
        })
        .init();
}

fn create_executable(
    fns: Vec<frontend::tokens::FunctionDeclaration>,
    struct_registry: StructRegistry,
    output: &str,
) {
    let bytes = compiler::compile(fns, struct_registry).unwrap();
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
