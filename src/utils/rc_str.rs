use std::{
    fmt::Display,
    ops::{Deref, Range},
    rc::Rc,
};

#[derive(Clone, Debug)]
pub struct RcStr {
    inner: Rc<str>,
    range: Range<usize>,
}

impl RcStr {
    pub fn new(s: String) -> Self {
        let range = 0..s.len();
        let inner: Rc<str> = s.into();

        Self { inner, range }
    }
    pub fn slice(&self, range: Range<usize>) -> Option<Self> {
        if self.range.start + range.end > self.inner.len() {
            return None;
        }
        let range = (self.range.start + range.start)..(self.range.start + range.end);
        Some(Self {
            inner: self.inner.clone(),
            range,
        })
    }

    pub fn trim(&self) -> Self {
        if let Some((word_start, _)) = self.char_indices().find(|(_, x)| !x.is_whitespace()) {
            if let Some((word_end, _)) = self.char_indices().rev().find(|(_, x)| x.is_whitespace()) {
                return self.slice(word_start..word_end).unwrap_or(self.clone());
            }
        }
        self.clone()
    }

    pub fn as_str(&self) -> &str {
        &self.inner[self.range.clone()]
    }

    pub fn len(&self) -> usize {
        self.range.len()
    }
}

impl Deref for RcStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl Display for RcStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl From<String> for RcStr {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl From<&str> for RcStr {
    fn from(value: &str) -> Self {
        Self::new(value.into())
    }
}
