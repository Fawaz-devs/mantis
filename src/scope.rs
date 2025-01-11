use std::{any::Any, ops::Deref};

use cranelift::{
    codegen::ir::Inst,
    prelude::{Block, FunctionBuilder, InstBuilder},
};
use cranelift_module::Module;
use cranelift_object::ObjectModule;

use crate::{
    frontend::tokens::{MsClScope, MsClScopeType, MsScopeType},
    ms::MsContext,
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

    pub fn exit_scope_until(&mut self, index: usize) {
        todo!()
    }

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

    pub fn add_variable(&mut self, var_name: impl Into<Box<str>>, var: MsVar) -> Option<MsVar> {
        let last_scope = self.scopes.last_mut().unwrap();
        let var_name: Box<str> = var_name.into();
        if let Some(old_variable) = last_scope.registry.insert(var_name.clone(), var) {
            let (idx, _) = last_scope
                .stack
                .iter()
                .enumerate()
                .find(|(_, x)| **x == var_name)
                .unwrap();
            last_scope.stack.remove(idx);
            last_scope.stack.push(var_name);
            return Some(old_variable);
        }

        last_scope.stack.push(var_name);
        None
    }
}

pub fn drop_scope(
    reg: &MsVarRegistry,
    ctx: &MsContext,
    fbx: &mut FunctionBuilder,
    module: &mut ObjectModule,
) {
    for var_name in reg.stack.iter().rev() {
        let var = reg.registry.get(var_name).unwrap();
        if !var.is_reference {
            drop_variable(var, ctx, fbx, module);
            log::info!("Dropped {} of type: {}", var_name, var.ty_id);
        }
    }
}

pub fn drop_variable(
    var: &MsVar,
    ctx: &MsContext,
    fbx: &mut FunctionBuilder,
    module: &mut ObjectModule,
) {
    let v = var;
    if let Some(drop_trait) = ctx
        .current_module
        .trait_registry
        .find_trait_for("Drop", v.ty_id)
    {
        let function = drop_trait.registry.get("drop").expect(&format!(
            "missing fn drop(self @mut Self); for {:?}",
            v.ty_id
        ));
        let func_ref = module.declare_func_in_func(function.func_id, fbx.func);
        let val = fbx.use_var(v.c_var);
        let _ = fbx.ins().call(func_ref, &[val]);
    }
}

pub fn drop_scopes_until_index(
    scope_index: usize,
    ctx: &MsContext,
    fbx: &mut FunctionBuilder,
    module: &mut ObjectModule,
) {
    for (idx, scope) in ctx.var_scopes.scopes.iter().enumerate().rev() {
        if idx <= scope_index {
            break;
        }
        drop_scope(scope, ctx, fbx, module);
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
    pub var_scope_index: usize,
}

#[derive(Debug, Default, Clone)]
pub struct MsLoopScopes {
    scopes: Vec<MsLoopScope>,
}

impl MsLoopScopes {
    pub fn new_loop(
        &mut self,
        name: Option<Box<str>>,
        fbx: &mut FunctionBuilder,
        var_scope_index: usize,
    ) -> &MsLoopScope {
        let scope = MsLoopScope {
            name,
            entry_block: fbx.create_block(),
            exit_block: fbx.create_block(),
            var_scope_index,
        };

        fbx.ins().jump(scope.entry_block, &[]);
        fbx.switch_to_block(scope.entry_block);
        self.scopes.push(scope);
        self.scopes.last().unwrap()
    }

    pub fn find_last_loop(&self, name: Option<&str>) -> Option<&MsLoopScope> {
        if let Some(name) = name {
            self.scopes.iter().rev().find(|x| {
                let Some(scope_name) = &x.name else {
                    return false;
                };

                scope_name.deref() == name
            })
        } else {
            self.scopes.last()
        }
    }

    pub fn end_loop(&mut self, name: Option<&str>, fbx: &mut FunctionBuilder) {
        let scope = self.find_last_loop(name).expect("unidentified loop");
        fbx.ins().jump(scope.entry_block, &[]);
        fbx.seal_block(scope.entry_block);

        fbx.switch_to_block(scope.exit_block);
        fbx.seal_block(scope.exit_block);
    }
    pub fn break_out_of_loop(&mut self, name: Option<&str>, fbx: &mut FunctionBuilder) -> Inst {
        let scope = self.find_last_loop(name).expect("unidentified loop");
        fbx.ins().jump(scope.exit_block, &[])
    }
    pub fn continue_loop(&mut self, name: Option<&str>, fbx: &mut FunctionBuilder) -> Inst {
        let scope = self.find_last_loop(name).expect("unidentified loop");
        fbx.ins().jump(scope.entry_block, &[])
    }
}
