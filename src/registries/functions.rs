use super::types::MsType;

#[derive(Clone, Debug, Copy)]
pub enum FunctionType {
    Extern,
    Private,
    Public,
}

#[derive(Clone, Debug)]
pub struct MsFunctionType {
    arguments: Vec<MsType>,
    return_value: Box<MsType>,
    fn_type: FunctionType,
}
