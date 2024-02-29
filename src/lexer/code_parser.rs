use std::{ops::Deref, rc::Rc};

use crate::utils::{bi_directional_iterator::BiDerectionalIterator, rc_str::RcStr, rc_vec::RcVec};

use super::types::Token;

pub struct CodeParser {
    inner: RcStr,
}

const BREAKPOINTS: &str = "{}()[],.+=-*/$#|&;\"'";

#[derive(Debug)]
pub enum CodeWord {
    BreakPoint(char),
    Word(RcStr),
}

impl CodeParser {
    pub fn new(s: RcStr) -> Self {
        // let inner: Rc<[char]> = s.chars().collect();
        // let inner = RcVec::from(inner);
        Self { inner: s }
    }

    pub fn parse(&self) -> Option<Vec<CodeWord>> {
        let mut tokens = Vec::<CodeWord>::with_capacity(self.inner.len() * 4);

        let mut parsed_till = 0;
        for (idx, c) in self.inner.char_indices() {
            let is_breakpoint = BREAKPOINTS.chars().any(|x| x == c);
            if is_breakpoint || c.is_whitespace() {
                print!("PI: {parsed_till} IDX: {idx} BP: {:x?}\n", c);
                if parsed_till < idx {
                    let word = self.inner.clone().slice(parsed_till..idx)?.trim();
                    tokens.push(CodeWord::Word(word));
                }
                if !c.is_whitespace() {
                    tokens.push(CodeWord::BreakPoint(c));
                }
                parsed_till = idx + 1;
            }
        }

        Some(tokens)
    }
}
