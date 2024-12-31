use std::collections::HashMap;

use crate::frontend::tokens::MsFunctionDeclaration;

use super::{types::MsType, MsRegistry, MsRegistryExt};

#[derive(Clone, Debug, Copy)]
pub enum FunctionType {
    Extern,
    Private,
    Public,
}

#[derive(Clone, Debug)]
pub struct MsFunctionType {
    pub arguments: Vec<MsType>,
    pub rets: MsType,
    pub fn_type: FunctionType,
}

impl From<&MsFunctionDeclaration> for MsFunctionType {
    fn from(value: &MsFunctionDeclaration) -> Self {
        Self {
            arguments: value.arguments.iter().map(|x| x.var_type.clone()).collect(),
            rets: value.return_type.clone(),
            fn_type: value.fn_type,
        }
    }
}

#[derive(Debug)]
pub struct MsFunctionRegistry {
    registry: HashMap<String, MsFunctionType>,
}

impl MsRegistry<MsFunctionType> for MsFunctionRegistry {
    fn get_registry(&self) -> &HashMap<String, MsFunctionType> {
        &self.registry
    }

    fn get_registry_mut(&mut self) -> &mut HashMap<String, MsFunctionType> {
        &mut self.registry
    }
}

impl MsRegistryExt<MsFunctionType> for MsFunctionRegistry {}

impl Default for MsFunctionRegistry {
    fn default() -> Self {
        let registry = HashMap::new();
        Self { registry }
    }
}

#[derive(Debug, Default)]
pub struct MsTraitRegistry {
    registry: HashMap<String, HashMap<String, MsFunctionType>>,
}

impl MsTraitRegistry {
    pub fn find(&self, trait_name: &str, ty: &MsType) -> Option<&MsFunctionRegistry> {
        todo!()
    }
}
