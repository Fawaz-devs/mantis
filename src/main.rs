#![allow(unused)]

use std::{collections::HashMap, io::Read};

use clap::Parser;
use cranelift::codegen::ir::types::I64;
use frontend::{
    tokenizer::{collect_functions, collect_to_tokens},
    tokens::MsFunctionDeclaration,
};
use lexer::lexer::Lexer;
use registries::{functions::MsFunctionRegistry, types::MsTypeRegistry};

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
mod native;
mod registries;
mod scope;
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
    obj: Option<String>,

    #[arg(long, short)]
    exe: Option<String>,
}

fn main() {
    init_logger();
    let args = Args::parse();
    handle0(args);

    // handle1(args);
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

fn handle0(args: Args) {
    let filepath = args.input;
    let input = std::fs::read_to_string(filepath).unwrap();

    let (fns, sr, fr) = collect_functions(input);

    if let Some(ast_path) = args.ast {
        std::fs::write(ast_path, format!("{:#?}\n{:#?}\n{:#?}", sr, fns, fr)).unwrap();
    } else {
        dbg!(&fns);
        dbg!(&sr);
        dbg!(&fr);
    }

    if let Some(obj_file_path) = args.obj {
        let bytes = compiler::ms_compile(fns, sr, fr).unwrap();
        std::fs::write(&obj_file_path, bytes).unwrap();
        if let Some(exe_file_path) = args.exe {
            let mut child = std::process::Command::new("gcc")
                .arg(obj_file_path)
                .arg("-o")
                .arg(&exe_file_path)
                .spawn()
                .unwrap();
            let exit_code = child.wait().unwrap();
            log::info!("gcc exit code: {}", exit_code);
            log::info!("compiled to {}", exe_file_path);
        }
    }
}
fn handle1(args: Args) {
    let filepath = args.input;
    let input = std::fs::read_to_string(filepath).unwrap();

    let (fns, sr) = read_to_tokens(input);

    if let Some(ast_path) = args.ast {
        std::fs::write(ast_path, format!("{:#?}\n{:#?}", sr, fns)).unwrap();
    } else {
        dbg!(&fns);
        dbg!(&sr);
    }

    if let Some(obj_file_path) = args.obj {
        let bytes = compiler::compile(fns, StructRegistry::new()).unwrap();
        std::fs::write(&obj_file_path, bytes).unwrap();
        if let Some(exe_file_path) = args.exe {
            let mut child = std::process::Command::new("gcc")
                .arg(obj_file_path)
                .arg("-o")
                .arg(&exe_file_path)
                .spawn()
                .unwrap();
            let exit_code = child.wait().unwrap();
            log::info!("gcc exit code: {}", exit_code);
            log::info!("compiled to {}", exe_file_path);
        }
    }
}
