#![allow(unused)]

use std::io::Read;

use lexer::lexer::Lexer;

use crate::{lexer::code_parser::{CodeParser, CodeWord}, utils::rc_str::RcStr};

mod lexer;
mod utils;

fn main() {
    let mut s = String::new();
    let file_path = std::env::args().skip(1).next().unwrap();
    let mut file = std::fs::OpenOptions::new()
        .read(true)
        .open(file_path)
        .unwrap();
    file.read_to_string(&mut s);

    let code_parser = CodeParser::new(s.into());
    let code_words = code_parser.parse().unwrap();

    code_words.iter().for_each(|x|  {
        match x {
            CodeWord::BreakPoint(bp) => print!("BP: {:x?}\n", bp),
            CodeWord::Word(word) => print!("WORD: '{word}'\n"),
        };
    });

    println!("");
}
