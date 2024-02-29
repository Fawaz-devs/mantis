use std::ops::Deref;

use crate::utils::{rc_str::RcStr, rc_vec::RcVec};

use super::types::Token;

pub struct TokenWrap {
    inner: RcVec<Token>,
    index: usize,
}

impl TokenWrap {
    pub fn new(inner: RcVec<Token>, index: usize) -> Option<Self> {
        let _ = inner.get(index)?;
        Some(Self {
            inner, index
        })
    }

    pub fn prev(&self) -> Option<Self> {
        let idx = self.index.checked_sub(1)?;
        Self::new(self.inner.clone(), idx)
    }

    pub fn next(&self) -> Option<Self> {
        Self::new(self.inner.clone(), self.index + 1)
    }

    pub fn get(&self) -> &Token {
        &self.inner[self.index]
    }
}

impl Deref for TokenWrap {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}


pub struct Parser {
    tokens: RcVec<Token>,
}

impl Parser {
    pub fn new(tokens: RcVec<Token>) -> Self {
        Self { tokens }
    }

    pub fn get(&self, idx: usize) -> Option<&Token> {
        self.tokens.get(idx)
    }
}
