use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

use cranelift::prelude::{Block, FunctionBuilder};

use crate::{
    frontend::tokens::{MsClScope, MsClScopeType, MsScopeType},
    registries::{
        functions::MsFunctionRegistry,
        types::MsTypeRegistry,
        variable::{MsVar, MsVarRegistry},
        MsRegistry, MsRegistryExt,
    },
};

#[derive(Debug)]
pub struct MsVarScopes {
    pub scopes: Vec<MsVarRegistry>,
}

impl MsVarScopes {
    pub fn new_scope(&mut self) -> usize {
        let index = self.scopes.len();
        self.scopes.push(MsVarRegistry::default());
        index
    }

    pub fn exit_scope(&mut self, fn_registry: &mut MsFunctionRegistry) -> Option<()> {
        let reg = self.scopes.pop()?;

        for (k, v) in reg.get_registry() {}

        Some(())
    }

    pub fn exit_scope_until(&mut self, index: usize) {}
}

#[derive(Debug)]
pub struct MsScopes {
    pub scopes: Vec<MsClScope>,
}

impl MsScopes {
    pub fn find_last_scope_of_type(&self, scope_ty: MsClScopeType) -> Option<&MsClScope> {
        for scope in self.scopes.iter().rev() {
            if scope.scope_type == scope_ty {
                return Some(scope);
            }
        }

        return None;
    }

    pub fn new_scope(&mut self, block: Block, scope_ty: MsClScopeType) {
        let scope = MsClScope::new(block, scope_ty);
        log::info!("New Scope {:?}", scope);
        self.scopes.push(scope);
    }

    pub fn exit_scope(&mut self) -> Option<MsClScope> {
        let scope = self.scopes.pop();
        log::info!("Exited Scope {:?}", scope);
        scope
    }

    pub fn last_scope(&self) -> Option<&MsClScope> {
        self.scopes.last()
    }

    pub fn last_scope_mut(&mut self) -> Option<&mut MsClScope> {
        self.scopes.last_mut()
    }

    pub fn get_variable(&self, var_name: &str) -> Option<&MsVar> {
        for scope in self.scopes.iter().rev() {
            match scope.variables.get_registry().get(var_name) {
                Some(var) => return Some(var),
                None => continue,
            };
        }

        None
    }

    pub fn get_last<'a>(
        &'a self,
        f: impl Fn(&'a MsClScope) -> Option<&'a MsClScope>,
    ) -> Option<&'a MsClScope> {
        for scope in self.scopes.iter().rev() {
            match f(&scope) {
                Some(val) => return Some(val),
                None => continue,
            }
        }

        None
    }
    pub fn get_last_mut<'a>(
        &'a self,
        f: impl Fn(&'a MsClScope) -> Option<&'a mut MsClScope>,
    ) -> Option<&'a mut MsClScope> {
        for scope in self.scopes.iter().rev() {
            match f(&scope) {
                Some(val) => return Some(val),
                None => continue,
            }
        }

        None
    }
}

impl Default for MsScopes {
    fn default() -> Self {
        Self { scopes: Vec::new() }
    }
}
