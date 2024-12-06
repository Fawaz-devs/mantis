use std::collections::HashMap;

use super::{types::MsType, MsRegistry};

#[derive(Clone, Debug)]
pub struct MsStructType {
    pub fields: HashMap<String, MsType>,
}
