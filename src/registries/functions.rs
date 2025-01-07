use std::{collections::HashMap, rc::Rc};

use cranelift_module::FuncId;
use linear_map::LinearMap;
use mantis_expression::pratt::{FunctionDecl, WordSpan};

use crate::frontend::tokens::MsFunctionDeclaration;

use super::{
    types::{MsType, MsTypeId},
    MsRegistry, MsRegistryExt,
};

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

#[derive(Clone, Debug)]
pub struct MsDeclaredFunction {
    pub arguments: LinearMap<WordSpan, mantis_expression::pratt::Type>, // var_name -> type
    pub rets: mantis_expression::pratt::Type,
    pub fn_type: FunctionType,
    pub func_id: FuncId,
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
    pub registry: HashMap<Box<str>, Rc<MsDeclaredFunction>>,
}

impl Default for MsFunctionRegistry {
    fn default() -> Self {
        let registry = HashMap::new();
        Self { registry }
    }
}

#[derive(Debug, Default)]
pub struct MsTraitRegistry {
    pub registry: HashMap<Box<str>, HashMap<MsTypeId, MsFunctionRegistry>>,
}

impl MsTraitRegistry {
    pub(crate) fn find_trait_for(
        &self,
        trait_name: &str,
        type_id: MsTypeId,
    ) -> Option<&MsFunctionRegistry> {
        let trait_registry = self.registry.get(trait_name)?;
        let functions = trait_registry.get(&type_id)?;

        Some(functions)
    }
}

#[derive(Default, Debug, Clone)]
pub struct MsGenericFunction {}
impl MsGenericFunction {
    pub(crate) fn generate(&self, real_types: Vec<super::modules::MsResolved>) -> FunctionDecl {
        todo!()
    }
}

#[derive(Default, Debug)]
pub struct MsFunctionTemplates {
    pub registry: HashMap<Box<str>, MsGenericFunction>,
}

#[derive(Default, Debug)]
pub struct MsTraitTemplates {}
