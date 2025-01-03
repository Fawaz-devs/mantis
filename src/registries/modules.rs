use std::{collections::HashMap, rc::Rc};

use mantis_expression::pratt::Type;

use super::{
    functions::{
        MsDeclaredFunction, MsFunctionRegistry, MsFunctionTemplates, MsTraitRegistry,
        MsTraitTemplates,
    },
    types::{MsType, MsTypeRegistry, MsTypeTemplates},
};

#[derive(Default, Debug)]
pub struct MsModuleRegistry {
    pub registry: HashMap<Box<str>, MsModule>, // path of module -> Registries
}

#[derive(Default, Debug)]
pub struct MsModule {
    pub fn_registry: MsFunctionRegistry,
    pub fn_templates: MsFunctionTemplates,
    pub trait_registry: MsTraitRegistry,
    pub trait_templates: MsTraitTemplates,
    pub type_registry: MsTypeRegistry,
    pub type_templates: MsTypeTemplates,

    pub submodules: HashMap<Box<str>, MsModule>,
}

#[derive(Debug, Clone)]
pub enum MsResolved {
    Function(Rc<MsDeclaredFunction>),
    Type(MsType),
}

impl MsModule {
    pub fn resolve(&mut self, type_name: &Type) -> Option<MsResolved> {
        match type_name {
            Type::WithGenerics(ty, generics) => {
                let generic_key = type_name.to_string();
                {
                    if let Some(ty) = self.type_registry.registry.get(&generic_key) {
                        return Some(MsResolved::Type(ty.clone()));
                    }
                    if let Some(func) = self.fn_registry.registry.get(&generic_key) {
                        return Some(MsResolved::Function(func.clone()));
                    }
                }
                {
                    let key = ty.to_string();
                    if let Some(template) = self.type_templates.registry.get(key.as_str()).cloned()
                    {
                        let mut real_types = generics
                            .iter()
                            .map(|x| self.resolve(x))
                            .collect::<Option<Vec<_>>>()?;

                        let generated_type = template.generate(real_types);
                        self.type_registry
                            .registry
                            .insert(generic_key, generated_type.clone());
                        return Some(MsResolved::Type(generated_type));
                    }
                    if let Some(template) = self.fn_templates.registry.get(key.as_str()).cloned() {
                        let mut real_types = generics
                            .iter()
                            .map(|x| self.resolve(x))
                            .collect::<Option<Vec<_>>>()?;

                        let generated_func = template.generate(real_types);
                        // self.type_registry
                        //     .registry
                        //     .insert(generic_key, generated_func.clone());
                        // return Some(MsResolved::Function(generated_func));
                        todo!("compile the generated func")
                    }
                }
                return None;
            }
            Type::Word(span) => {
                let key = span.as_str();
                if let Some(ty) = self.type_registry.registry.get(key) {
                    return Some(MsResolved::Type(ty.clone()));
                }
                if let Some(func) = self.fn_registry.registry.get(key) {
                    return Some(MsResolved::Function(func.clone()));
                }
                return None;
            }
            Type::Nested(root, child) => {
                let key = root.to_string();
                let module = self.submodules.get_mut(key.as_str())?;
                return module.resolve(child);
            }
            _ => unreachable!("unhandled"),
        };

        todo!()
    }
}
