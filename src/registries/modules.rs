use std::{
    collections::HashMap,
    fs::FileType,
    path::{Path, PathBuf},
    rc::Rc,
};

use linear_map::LinearMap;
use mantis_expression::pratt::Type;

use crate::{
    backend::compile_function::random_string,
    native::instructions::Either,
    registries::{structs::MsStructType, types::StructWithGenerics},
};

use super::{
    functions::{
        MsDeclaredFunction, MsFunctionRegistry, MsFunctionTemplates, MsTraitGenericTemplates,
        MsTraitRegistry, MsTraitTemplates,
    },
    types::{
        EnumWithGenerics, MsGenericTemplate, MsGenericTemplateInner, MsType, MsTypeId,
        MsTypeNameRegistry, MsTypeRegistry, MsTypeTemplates, MsTypeWithId, TypeNameWithGenerics,
    },
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
    pub type_registry: MsTypeNameRegistry,
    pub type_templates: MsTypeTemplates,
    pub type_fn_registry: MsTypeFunctionRegistry,
    pub trait_generic_templates: MsTraitGenericTemplates,
    pub submodules: HashMap<Box<str>, MsModule>,
    pub aliased_types: HashMap<TypeNameWithGenerics, MsTypeWithId>,
}

#[derive(Debug, Default)]
pub struct MsTypeFunctionRegistry {
    pub map: HashMap<MsTypeId, MsFunctionRegistry>,
}

impl MsTypeFunctionRegistry {
    pub fn add_function(
        &mut self,
        ty: MsTypeId,
        fn_name: impl Into<Box<str>>,
        func: Rc<MsDeclaredFunction>,
    ) {
        let ty = self.map.get_mut(&ty).unwrap();
        ty.add_function(fn_name, func);
    }
}

#[derive(Debug, Clone)]
pub enum MsResolved {
    Function(Rc<MsDeclaredFunction>),
    Type(MsTypeWithId),
    TypeRef(MsTypeWithId, bool),
    Generic(Rc<MsGenericTemplate>),
    EnumUnwrap(MsTypeWithId, Box<str>), // enum_ty and variant name
}

impl MsResolved {
    pub fn ty(&self) -> Option<MsTypeWithId> {
        match self {
            MsResolved::Type(ms_type) => Some(ms_type.clone()),
            MsResolved::TypeRef(ms_type, _) => Some(ms_type.clone()),
            _ => None,
        }
    }

    pub fn is_reference(&self) -> bool {
        matches!(self, MsResolved::TypeRef(_, __))
    }
}

impl MsModule {
    pub fn clear_aliases(&mut self) {
        self.aliased_types.clear();
    }

    pub fn add_alias(&mut self, alias_name: TypeNameWithGenerics, alias_type: MsTypeWithId) {
        assert!(self.aliased_types.insert(alias_name, alias_type).is_none())
    }

    pub fn resolve_from_str(&mut self, type_name: &str) -> Option<MsResolved> {
        self.resolve(&Type::Word(type_name.into()))
    }

    pub fn resolve(&mut self, type_name: &Type) -> Option<MsResolved> {
        if let Some(ty_name) = TypeNameWithGenerics::from_type(type_name) {
            if let Some(resolved) = self.aliased_types.get(&ty_name) {
                return Some(MsResolved::Type(resolved.clone()));
            }
        }

        match type_name {
            Type::WithGenerics(ty, generics) => {
                {
                    let generic_key = type_name.to_string();
                    if let Some(ty) = self.type_registry.get_from_str(&generic_key) {
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
                        let mut real_types = HashMap::<Box<str>, MsTypeWithId>::new();

                        for (generic_name, ty) in template.generics.iter().zip(generics.iter()) {
                            if let Some(MsResolved::Type(real_ty)) = self.resolve(ty) {
                                real_types.insert(generic_name.as_str().into(), real_ty);
                            }
                        }
                        dbg!(&real_types);
                        let generated_type = template.generate(&real_types, self);
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
                if let Some(ty) = self.type_registry.get_from_str(key) {
                    return Some(MsResolved::Type(ty.clone()));
                }
                if let Some(func) = self.fn_registry.registry.get(key) {
                    return Some(MsResolved::Function(func.clone()));
                }
                return None;
            }
            Type::Nested(root, child) => {
                if let Some(MsResolved::Type(ty)) = self.resolve(root) {
                    // let self_traits = self.trait_registry.registry.get("Self")?;
                    match &ty.ty {
                        MsType::Enum(enum_ty) => {
                            let variant_name = child.word()?;
                            return Some(MsResolved::EnumUnwrap(ty, variant_name.into()));
                        }
                        _ => {}
                    }
                    let func = self
                        .type_fn_registry
                        .map
                        .get(&ty.id)?
                        .registry
                        .get(child.word()?)?;
                    return Some(MsResolved::Function(func.clone()));
                }

                let key = root.to_string();

                let module = self.submodules.get_mut(key.as_str())?;
                return module.resolve(child);
            }
            Type::Struct { fields } => {
                let mut struct_ty = MsStructType::default();

                for (key, value) in fields {
                    let field_name = key.as_str();
                    let Some(MsResolved::Type(field_ty)) = self.resolve(value) else {
                        panic!("undefined type {}: {:?}", field_name, value);
                    };
                    struct_ty.add_field(field_name, field_ty);
                }

                let struct_name = random_string(24);
                let ty = MsType::Struct(Rc::new(struct_ty));
                let id = self.type_registry.add_type(struct_name, ty.clone());

                return Some(MsResolved::Type(MsTypeWithId { id, ty }));
            }

            Type::Ref(ty, is_mutable) => {
                let ty = self.resolve(ty)?.ty().unwrap();
                let ty = MsResolved::TypeRef(ty, *is_mutable);
                return Some(ty);
            }

            _ => unreachable!("unhandled {:?}", type_name),
        };
        // return None;
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
                    inner_type: MsGenericTemplateInner::Type(
                        TypeNameWithGenerics::from_type(type_name).unwrap(),
                    ),
                };

                return template;
            }
            Type::Word(span) => {
                let template = MsGenericTemplate {
                    generics: root_generices.to_vec(),
                    inner_type: MsGenericTemplateInner::Type(
                        TypeNameWithGenerics::from_type(type_name).unwrap(),
                    ),
                };

                return template;
            }
            Type::Struct { fields } => {
                let mut map = LinearMap::<Box<str>, TypeNameWithGenerics>::new();

                for (key, value) in fields {
                    let ty = TypeNameWithGenerics::from_type(value).unwrap();
                    map.insert(key.as_str().into(), ty);
                }

                let template = MsGenericTemplate {
                    generics: root_generices.to_vec(),
                    inner_type: MsGenericTemplateInner::Struct(StructWithGenerics { map }),
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
            Type::Enum { fields } => {
                let mut map = LinearMap::<Box<str>, Option<TypeNameWithGenerics>>::new();

                for (variant_name, fields) in fields {
                    let argument = fields
                        .first()
                        .and_then(|x| TypeNameWithGenerics::from_type(x));

                    map.insert(variant_name.as_str().into(), argument);
                }

                let inner = MsGenericTemplateInner::Enum(EnumWithGenerics { map });

                let template = MsGenericTemplate {
                    generics: root_generices.to_vec(),
                    inner_type: inner,
                };
                return template;
            }
            _ => unreachable!("unhandled {:?}", type_name),
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
