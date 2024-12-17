use std::collections::HashMap;

use cranelift::prelude::FunctionBuilder;
use cranelift_object::ObjectModule;
use mantis_expression::node::{BinaryOperation, Node};

use crate::{
    registries::{types::MsTypeRegistry, variable::MsVarRegistry},
    scope::MsScopes,
};
