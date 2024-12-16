use crate::registries::{
    types::MsTypeRegistry, variable::MsVarRegistry, MsRegistry, MsRegistryExt,
};

#[derive(Default, Debug)]
pub struct MsScopes<T> {
    scopes: Vec<T>,
}

impl<T> MsScopes<T>
where
    T: Default,
{
    pub fn new_scope(&mut self) {
        self.scopes.push(T::default());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn last_scope(&self) -> Option<&T> {
        self.scopes.last()
    }

    pub fn last_scope_mut(&mut self) -> &mut T {
        if self.scopes.is_empty() {
            self.scopes.push(T::default());
        }

        self.scopes.last_mut().unwrap()
    }

    pub fn get_last<'a>(&'a self, f: impl Fn(&'a T) -> Option<&'a T>) -> Option<&'a T> {
        for scope in self.scopes.iter().rev() {
            match f(&scope) {
                Some(val) => return Some(val),
                None => continue,
            }
        }

        None
    }
    pub fn get_last_mut<'a>(&'a self, f: impl Fn(&'a T) -> Option<&'a mut T>) -> Option<&'a mut T> {
        for scope in self.scopes.iter().rev() {
            match f(&scope) {
                Some(val) => return Some(val),
                None => continue,
            }
        }

        None
    }
}
