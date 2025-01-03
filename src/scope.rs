use cranelift::prelude::{Block, FunctionBuilder, InstBuilder};
use cranelift_module::Module;
use cranelift_object::ObjectModule;

use crate::{
    frontend::tokens::{MsClScope, MsClScopeType, MsContext, MsScopeType},
    registries::variable::{MsVar, MsVarRegistry},
};

#[derive(Debug, Default)]
pub struct MsVarScopes {
    pub scopes: Vec<MsVarRegistry>,
}

impl MsVarScopes {
    pub fn new_scope(&mut self) -> usize {
        let index = self.scopes.len();
        self.scopes.push(MsVarRegistry::default());
        index
    }

    pub fn exit_scope_until(&mut self, index: usize) {}

    pub fn exit_scope(&mut self) -> Option<MsVarRegistry> {
        self.scopes.pop()
    }

    pub fn find_variable(&self, name: &str) -> Option<&MsVar> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.registry.get(name) {
                return Some(var);
            }
        }
        return None;
    }

    pub fn add_variable(&mut self, var_name: impl Into<String>, var: MsVar) -> Option<MsVar> {
        self.scopes
            .last_mut()
            .unwrap()
            .registry
            .insert(var_name.into(), var)
    }
}

pub fn drop_scope(
    reg: MsVarRegistry,
    ctx: &MsContext,
    fbx: &mut FunctionBuilder,
    module: &mut ObjectModule,
) {
    for (k, v) in reg.registry.into_iter().rev() {
        drop_variable(v, ctx, fbx, module);
        log::info!("Dropped {}", k);
    }
}

pub fn drop_variable(
    var: MsVar,
    ctx: &MsContext,
    fbx: &mut FunctionBuilder,
    module: &mut ObjectModule,
) {
    let v = var;
    if let Some(drop_trait) = ctx.trait_registry.find_trait_for("Drop", &v.ty.to_string()) {
        let function = drop_trait
            .registry
            .get("drop")
            .expect(&format!("missing fn drop(self @mut Self); for {:?}", v.ty));
        let func_ref = module.declare_func_in_func(function.func_id, fbx.func);
        let val = fbx.use_var(v.c_var);
        let _ = fbx.ins().call(func_ref, &[val]);
    }
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
            match scope.variables.registry.get(var_name) {
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

#[derive(Debug, Clone)]
pub struct MsLoopScope {
    pub name: Option<Box<str>>,
    pub entry_block: Block,
    pub exit_block: Block,
}

#[derive(Debug, Default, Clone)]
pub struct MsLoopScopes {
    pub scopes: Vec<MsLoopScope>,
}
