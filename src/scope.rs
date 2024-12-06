use crate::registries::{types::TypeRegistry, variable::MsVarRegistry, MsRegistry, MsRegistryExt};

pub struct MsScope {
    registries: Vec<MsVarRegistry>,
}

impl MsScope {
    pub fn new_scope(&mut self) {
        self.registries.push(MsVarRegistry::default());
    }

    pub fn exit_scope(&mut self) {
        self.registries.pop();
    }

    pub fn get_variable(&self, var_name: &str) {
        for reg in self.registries.iter().rev() {
            if let Some(var_type) = reg.get(var_name) {}
        }
    }
}
