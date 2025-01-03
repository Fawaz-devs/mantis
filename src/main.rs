#![allow(unused)]

use std::rc::Rc;

use clap::Parser;

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

    // Output .o file
    #[arg(long, short)]
    obj: Option<String>,

    #[arg(long, short)]
    exe: Option<String>,

    #[arg(long, short)]
    lib: Option<String>,

    #[arg(long, short)]
    static_lib: bool,

    #[arg(long, short)]
    shared_lib: bool,

    #[arg(long, short)]
    module_name: Option<String>,
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

    let src = Rc::from(input.clone());

    let declarations = mantis_expression::pratt::parse_blocks(&src).expect("Unparsed declarations");
    let include_dirs = Vec::new();
    // let (fns, sr) = collect_functions(input);

    if let Some(ast_path) = args.ast {
        let mut f = std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .open(ast_path)
            .unwrap();

        use std::io::Write;

        write!(f, "{:#?}", declarations).unwrap();
        // std::fs::write(ast_path, format!("{:#?}", declarations)).unwrap();
    } else {
        dbg!(&declarations);
    }

    if let Some(obj_file_path) = args.obj {
        {
            let module_name = args.module_name.unwrap_or("main".to_string());
            let bytes =
                backend::compile::compile_binary(declarations, include_dirs, &module_name).unwrap();
            // let bytes = compiler::ms_compile(fns, sr, fr).unwrap();
            std::fs::write(&obj_file_path, &bytes).unwrap();
            log::info!("wrote {} bytes {}", bytes.len(), obj_file_path);
        }
        let mut cmd_args = Vec::with_capacity(6);
        cmd_args.push(obj_file_path.as_str());

        if let Some(exe_file_path) = &args.exe {
            cmd_args.push("-o");
            cmd_args.push(exe_file_path);
        } else if let Some(lib_file_path) = &args.lib {
            if args.static_lib {
                cmd_args.push("-static");
            } else if args.shared_lib {
                cmd_args.push("-shared");
            }
            cmd_args.push("-o");
            cmd_args.push(lib_file_path);
        }

        if cmd_args.len() > 1 {
            log::info!("running cc {}", cmd_args.join(" "));
            let mut child = std::process::Command::new("cc")
                .args(cmd_args)
                .spawn()
                .unwrap();
            let exit_code = child.wait().unwrap();
            log::info!("cc exit code: {}", exit_code);
        }
    }
}
// fn handle1(args: Args) {
//     let filepath = args.input;
//     let input = std::fs::read_to_string(filepath).unwrap();

//     let (fns, sr) = read_to_tokens(input);

//     if let Some(ast_path) = args.ast {
//         std::fs::write(ast_path, format!("{:#?}\n{:#?}", sr, fns)).unwrap();
//     } else {
//         dbg!(&fns);
//         dbg!(&sr);
//     }

//     if let Some(obj_file_path) = args.obj {
//         let bytes = compiler::compile(fns, StructRegistry::new()).unwrap();
//         std::fs::write(&obj_file_path, bytes).unwrap();
//         if let Some(exe_file_path) = args.exe {
//             let mut child = std::process::Command::new("gcc")
//                 .arg(obj_file_path)
//                 .arg("-o")
//                 .arg(&exe_file_path)
//                 .spawn()
//                 .unwrap();
//             let exit_code = child.wait().unwrap();
//             log::info!("gcc exit code: {}", exit_code);
//             log::info!("compiled to {}", exe_file_path);
//         }
//     }
// }
