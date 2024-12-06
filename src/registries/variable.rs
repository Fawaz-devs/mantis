use std::collections::HashMap;

use super::{MsRegistry, MsRegistryExt};

#[derive(Default, Clone, Debug)]
pub struct MsVarRegistry {
    registry: HashMap<String, String>, // variable name -> variable type name
}

impl MsRegistry<String> for MsVarRegistry {
    fn get_registry(&self) -> &HashMap<String, String> {
        &self.registry
    }

    fn get_registry_mut(&mut self) -> &mut HashMap<String, String> {
        &mut self.registry
    }
}

impl MsRegistryExt<String> for MsVarRegistry {}
