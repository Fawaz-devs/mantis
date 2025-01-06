use std::{
    collections::HashMap,
    fs::FileType,
    path::{Path, PathBuf},
    rc::Rc,
};

use linear_map::LinearMap;
use mantis_expression::pratt::Type;

use crate::{native::instructions::Either, registries::types::StructWithGenerics};

use super::{
    functions::{
        MsDeclaredFunction, MsFunctionRegistry, MsFunctionTemplates, MsTraitRegistry,
        MsTraitTemplates,
    },
    types::{MsGenericTemplate, MsType, MsTypeRegistry, MsTypeTemplates, TypeNameWithGenerics},
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
    Generic(Rc<MsGenericTemplate>),
}

impl MsResolved {
    pub fn ty(&self) -> Option<MsType> {
        match self {
            MsResolved::Type(ms_type) => Some(ms_type.clone()),
            _ => None,
        }
    }
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
                    if let Some(func) = self.fn_registry.registry.get(generic_key.as_str()) {
                        return Some(MsResolved::Function(func.clone()));
                    }
                }
                {
                    let key = ty.to_string();

                    if let Some(template) = self.type_templates.registry.get(key.as_str()).cloned()
                    {
                        log::info!("found template {}, generating struct", key);
                        let mut real_types = HashMap::<Box<str>, MsType>::new();

                        for (generic_name, ty) in template.generics.iter().zip(generics.iter()) {
                            if let Some(MsResolved::Type(real_ty)) = self.resolve(ty) {
                                real_types.insert(generic_name.as_str().into(), real_ty);
                            }
                        }

                        let generated_type = template.generate(&real_types, &self);
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
            Type::Struct { fields } => {
                for (key, value) in fields {}
                todo!()
            }
            _ => unreachable!("unhandled"),
        };
        return None;
    }

    pub fn resolve_with_generics(
        &mut self,
        type_name: &Type,
        root_generices: &[String],
    ) -> MsGenericTemplate {
        match type_name {
            Type::WithGenerics(ty, generics) => {
                let template = MsGenericTemplate {
                    generics: root_generices.to_vec(),
                    inner_type: Either::Left(TypeNameWithGenerics::from_type(type_name)),
                };

                return template;
            }
            Type::Word(span) => {
                let template = MsGenericTemplate {
                    generics: root_generices.to_vec(),
                    inner_type: Either::Left(TypeNameWithGenerics::from_type(type_name)),
                };

                return template;
            }
            Type::Struct { fields } => {
                let mut map = LinearMap::new();

                for (key, value) in fields {
                    let ty = TypeNameWithGenerics::from_type(value);
                    map.insert(key.as_str().into(), ty);
                }

                let template = MsGenericTemplate {
                    generics: root_generices.to_vec(),
                    inner_type: Either::Right(StructWithGenerics { map }),
                };

                return template;
            }
            Type::Nested(root, child) => {
                let key = root.to_string();
                let module = self
                    .submodules
                    .get_mut(key.as_str())
                    .expect("can't find module");
                return module.resolve_with_generics(child, root_generices);
            }

            _ => unreachable!("unhandled"),
        };
    }
}

pub enum ModuleEntry {
    Module(String),
    Dir(PathBuf),
}

pub fn resolve_module_by_word(include_dirs: &[String], module_name: &str) -> Option<ModuleEntry> {
    for dir_path in include_dirs {
        let dir = match std::fs::read_dir(dir_path) {
            Ok(d) => d,
            Err(err) => {
                log::warn!("unable to read dir {}, error: {:?}", dir_path, err);
                continue;
            }
        };
        for entity in dir {
            let entry = match entity {
                Ok(d) => d,
                Err(err) => {
                    log::warn!("unable to read dir {}, error: {:?}", dir_path, err);
                    continue;
                }
            };
            let entry_name = entry.file_name();
            let file_name = entry_name.to_str().expect("invalid os string");
            if file_name == module_name && entry.file_type().unwrap().is_dir() {
                return Some(ModuleEntry::Dir(entry.path()));
            } else if file_name.len() == module_name.len() + 3
                && file_name.starts_with(module_name)
                && file_name.ends_with(".ms")
            {
                let content = std::fs::read_to_string(entry.path())
                    .expect(&format!("Failed to read {:?}", entry.path()));
                return Some(ModuleEntry::Module(content));
            }
        }
    }
    return None;
}
