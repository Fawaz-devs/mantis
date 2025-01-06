use cranelift::prelude::*;
use cranelift_module::{DataDescription, FuncOrDataId, Linkage, Module};
use cranelift_object::ObjectModule;
use mantis_expression::node::BinaryOperation;
use mantis_tokens::MantisLexerTokens;

use crate::registries::{
    types::MsType,
    variable::{MsVal, MsVar},
};

// use crate::{
//     frontend::tokens::{MsContext, MsNode},
//     registries::{
//         types::{MsNativeType, MsType},
//         variable::{MsVal, MsVar},
//         MsRegistryExt,
//     },
// };

// // fn translate_binary_op(
// //     op: BinaryOperation,
// //     lhs: &mantis_expression::node::Node,
// //     rhs: &mantis_expression::node::Node,

// //     ms_ctx: &mut MsContext,
// //     fbx: &mut FunctionBuilder<'_>,
// //     module: &mut ObjectModule,
// // ) -> NodeResult {
// //     match op {
// //         BinaryOperation::Add => {
// //             let lhs = translate_node(lhs, ms_ctx, fbx, module);
// //             let rhs = translate_node(rhs, ms_ctx, fbx, module);
// //             if !lhs.ty().equal(&rhs.ty()) {
// //                 panic!(
// //                     "binary operation on non similar types {:?} != {:?}",
// //                     lhs.ty(),
// //                     rhs.ty()
// //                 );
// //             }

// //             match lhs.ty() {
// //                 MsType::Native(nty) => {
// //                     return NodeResult::Val(MsVal::new(
// //                         nty.add(lhs.value(fbx), rhs.value(fbx), fbx),
// //                         lhs.ty().clone(),
// //                     ))
// //                 }
// //                 _ => todo!(),
// //             };
// //         }
// //         BinaryOperation::Sub => {
// //             let lhs = translate_node(lhs, ms_ctx, fbx, module);
// //             let rhs = translate_node(rhs, ms_ctx, fbx, module);
// //             if !lhs.ty().equal(&rhs.ty()) {
// //                 panic!(
// //                     "binary operation on non similar types {:?} != {:?}",
// //                     lhs.ty(),
// //                     rhs.ty()
// //                 );
// //             }

// //             match lhs.ty() {
// //                 MsType::Native(nty) => {
// //                     return NodeResult::Val(MsVal::new(
// //                         nty.sub(lhs.value(fbx), rhs.value(fbx), fbx),
// //                         lhs.ty().clone(),
// //                     ))
// //                 }
// //                 _ => todo!(),
// //             };
// //         }
// //         BinaryOperation::Div => {
// //             let lhs = translate_node(lhs, ms_ctx, fbx, module);
// //             let rhs = translate_node(rhs, ms_ctx, fbx, module);
// //             if !lhs.ty().equal(&rhs.ty()) {
// //                 panic!(
// //                     "binary operation on non similar types {:?} != {:?}",
// //                     lhs.ty(),
// //                     rhs.ty()
// //                 );
// //             }

// //             match lhs.ty() {
// //                 MsType::Native(nty) => {
// //                     return NodeResult::Val(MsVal::new(
// //                         nty.div(lhs.value(fbx), rhs.value(fbx), fbx),
// //                         lhs.ty().clone(),
// //                     ))
// //                 }
// //                 _ => todo!(),
// //             };
// //         }
// //         BinaryOperation::Mult => {
// //             let lhs = translate_node(lhs, ms_ctx, fbx, module);
// //             let rhs = translate_node(rhs, ms_ctx, fbx, module);
// //             if !lhs.ty().equal(&rhs.ty()) {
// //                 panic!(
// //                     "binary operation on non similar types {:?} != {:?}",
// //                     lhs.ty(),
// //                     rhs.ty()
// //                 );
// //             }

// //             match lhs.ty() {
// //                 MsType::Native(nty) => {
// //                     return NodeResult::Val(MsVal::new(
// //                         nty.mult(lhs.value(fbx), rhs.value(fbx), fbx),
// //                         lhs.ty().clone(),
// //                     ))
// //                 }
// //                 _ => todo!(),
// //             };
// //         }

// //         BinaryOperation::GreaterThan
// //         | BinaryOperation::GreaterThanOrEqualTo
// //         | BinaryOperation::EqualTo
// //         | BinaryOperation::NotEqualTo
// //         | BinaryOperation::LessThan
// //         | BinaryOperation::LessThanOrEqualTo => {
// //             let lhs = translate_node(lhs, ms_ctx, fbx, module);
// //             let rhs = translate_node(rhs, ms_ctx, fbx, module);
// //             if !lhs.ty().equal(&rhs.ty()) {
// //                 panic!(
// //                     "binary operation on non similar types {:?} != {:?}",
// //                     lhs.ty(),
// //                     rhs.ty()
// //                 );
// //             }

// //             match lhs.ty() {
// //                 MsType::Native(nty) => {
// //                     return NodeResult::Val(MsVal::new(
// //                         nty.compare(op, lhs.value(fbx), rhs.value(fbx), fbx),
// //                         lhs.ty().clone(),
// //                     ))
// //                 }
// //                 _ => todo!(),
// //             };
// //         }

// //         BinaryOperation::Assign => {
// //             let l = translate_node(lhs, ms_ctx, fbx, module);
// //             let r = translate_node(rhs, ms_ctx, fbx, module);
// //             if !l.ty().equal(&r.ty()) {
// //                 panic!(
// //                     "binary operation on non similar types {:?} of ty {:?} != {:?} of ty {:?}",
// //                     lhs, l, rhs, r,
// //                 );
// //             }

// //             match l {
// //                 NodeResult::Var(var) => match var.ty {
// //                     MsType::Native(nty) => {
// //                         let val = r.value(fbx);
// //                         fbx.def_var(var.c_var, val);

// //                         return NodeResult::Val(MsVal::new(val, var.ty));
// //                     }
// //                     _ => todo!(),
// //                 },
// //                 NodeResult::Val(_) => panic!("Value can't be assigned by another value"),
// //                 NodeResult::StructAccessVar { ptr, offset } => {
// //                     let rvalue = r.value(fbx);
// //                     let pvalue = ptr.value();
// //                     let inst = fbx
// //                         .ins()
// //                         .store(MemFlags::new(), rvalue, pvalue, offset as i32);

// //                     return NodeResult::Val(MsVal::new(rvalue, r.ty().clone()));
// //                 }
// //             }
// //         }

// //         BinaryOperation::Cast => {
// //             let MsNode::Var(MantisLexerTokens::Word(type_name)) = rhs else {
// //                 panic!("RHS is not type");
// //             };
// //             let l = translate_node(lhs, ms_ctx, fbx, module);

// //             let r = ms_ctx
// //                 .type_registry
// //                 .get(&type_name)
// //                 .expect("undefined type name");

// //             match l.ty() {
// //                 MsType::Native(nty) => {
// //                     return NodeResult::Val(MsVal::new(
// //                         nty.cast_to(l.value(fbx), r, fbx),
// //                         r.clone(),
// //                     ))
// //                 }
// //                 _ => todo!(),
// //             }
// //         }
// //         BinaryOperation::Call => {
// //             let MsNode::Var(MantisLexerTokens::Word(fn_name)) = lhs else {
// //                 panic!("No functoin name as token for call expr");
// //             };

// //             let MsNode::Tuple(args) = rhs else {
// //                 panic!("function call expects arguments");
// //             };

// //             let Some(FuncOrDataId::Func(fn_id)) = module.get_name(&fn_name) else {
// //                 panic!("no function with that name is declared");
// //             };

// //             let fn_ref = module.declare_func_in_func(fn_id, fbx.func);

// //             let values = args
// //                 .iter()
// //                 .map(|x| translate_node(x, ms_ctx, fbx, module).value(fbx))
// //                 .collect::<Vec<_>>();

// //             let inst = fbx.ins().call(fn_ref, &values);
// //             let results = fbx.inst_results(inst);

// //             if !results.is_empty() {
// //                 let value = results[0];

// //                 let signature = ms_ctx
// //                     .fn_registry
// //                     .registry
// //                     .get(fn_name.as_str())
// //                     .expect("Undeclared function");

// //                 log::info!("Calling {} with signature {:?}", fn_name, signature);
// //                 // return NodeResult::Val(MsVal::new(value, signature.rets.clone()));
// //                 todo!()
// //             }
// //         }
// //         BinaryOperation::Access => {
// //             let lhs = translate_node(lhs, ms_ctx, fbx, module);

// //             match lhs.ty() {
// //                 MsType::Struct(st) => {
// //                     let MsNode::Var(MantisLexerTokens::Word(field_name)) = rhs else {
// //                         panic!("No Field Name on rhs {:?}", rhs);
// //                     };

// //                     let field = st.get_field(&field_name).expect("Undefined field name");
// //                     // let value = fbx.ins().load(
// //                     //     field.ty.to_cl_type().unwrap(),
// //                     //     MemFlags::new(),
// //                     //     ptr,
// //                     //     field.offset as i32,
// //                     // );

// //                     return NodeResult::StructAccessVar {
// //                         ptr: MsVal::new(lhs.value(fbx), field.ty.clone()),
// //                         offset: field.offset as u32,
// //                     };
// //                 }
// //                 _ => panic!("What we accessing if not struct?"),
// //             }
// //         }
// //         BinaryOperation::Empty => todo!(),
// //     }

// //     todo!("{:?} {:?} {:?}", op, lhs, rhs)
// // }

#[derive(Debug, Clone)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

// impl Either<MsVal, MsVar> {
//     pub fn value(&self, fbx: &mut FunctionBuilder) -> Value {
//         match self {
//             Either::Left(val) => val.value,
//             Either::Right(var) => fbx.use_var(var.c_var),
//         }
//     }

//     pub fn ty(&self) -> &MsType {
//         match self {
//             Either::Left(val) => &val.ty,
//             Either::Right(var) => &var.ty,
//         }
//     }
// }

#[derive(Debug, Clone)]
pub enum NodeResult {
    Val(MsVal),
    Var(MsVar),
    StructAccessVar { ptr: MsVal, offset: u32 },
}

impl NodeResult {
    pub fn value(&self, fbx: &mut FunctionBuilder) -> Value {
        match self {
            NodeResult::Val(val) => val.value,
            NodeResult::Var(var) => fbx.use_var(var.c_var),
            NodeResult::StructAccessVar { ptr, offset } => {
                let value = ptr.value();
                fbx.ins().load(
                    ptr.ty.to_cl_type().unwrap(),
                    MemFlags::new(),
                    value,
                    *offset as i32,
                )
            }
        }
    }

    pub fn ty(&self) -> &MsType {
        match self {
            NodeResult::Val(val) => &val.ty,
            NodeResult::Var(var) => &var.ty,
            NodeResult::StructAccessVar { ptr, offset } => &ptr.ty,
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            NodeResult::Val(ms_val) => ms_val.type_name(),
            NodeResult::Var(ms_var) => ms_var.type_name(),
            NodeResult::StructAccessVar { ptr, offset } => todo!(),
        }
    }

    pub fn to_ms_val(&self, fbx: &mut FunctionBuilder) -> MsVal {
        let ty = self.ty().clone();
        let val = self.value(fbx);
        MsVal::new(val, ty, self.type_name())
    }
}

// pub fn translate_node(
//     node: &MsNode,
//     ms_ctx: &mut MsContext,
//     fbx: &mut FunctionBuilder<'_>,
//     module: &mut ObjectModule,
// ) -> NodeResult {
//     use mantis_expression::node::Node;

//     match node {
//         Node::Binary(op, lhs, rhs) => {
//             return translate_binary_op(
//                 op.clone(),
//                 lhs.as_ref(),
//                 rhs.as_ref(),
//                 ms_ctx,
//                 fbx,
//                 module,
//             );
//         }
//         Node::Var(var_token) => {
//             match var_token {
//                 MantisLexerTokens::Word(var_name) => {
//                     let var = ms_ctx
//                         .scopes
//                         .get_variable(var_name)
//                         .expect("Undeclared variable");

//                     return NodeResult::Var(MsVar::new(var.ty.clone(), var.c_var));
//                 }
//                 MantisLexerTokens::Integer(int) => {
//                     let val = fbx.ins().iconst(types::I64, *int);
//                     let ty = MsType::Native(MsNativeType::I64);
//                     return NodeResult::Val(MsVal::new(val, ty));
//                 }

//                 MantisLexerTokens::Float(float) => {
//                     let val = fbx.ins().f64const(*float);
//                     let ty = MsType::Native(MsNativeType::F64);
//                     return NodeResult::Val(MsVal::new(val, ty));
//                 }

//                 MantisLexerTokens::String(s) => {
//                     let data_id = if let Some(FuncOrDataId::Data(data_id)) = module.get_name(s) {
//                         data_id
//                     } else {
//                         let data_id = module
//                             .declare_data(s, Linkage::Local, false, false)
//                             .unwrap();
//                         let mut data_description = DataDescription::new();
//                         data_description.define(s.as_bytes().into());
//                         module.define_data(data_id, &data_description);

//                         // let gl_value = module.declare_data_in_func(data_id, fbx.func);
//                         // let val = fbx.ins().global_value(types::I64, gl_value);

//                         // let st = ms_ctx.type_registry.get("array").unwrap();
//                         data_id
//                     };

//                     let gl_value = module.declare_data_in_func(data_id, fbx.func);
//                     ms_ctx.type_registry.get("array").unwrap().clone();
//                 }
//                 _ => panic!("Unsupported variable token {:?}", var_token),
//             };
//         }
//         Node::Expr(inner_node) => return translate_node(inner_node.as_ref(), ms_ctx, fbx, module),
//         _ => {}
//     };

//     let null = fbx.ins().null(types::I32);
//     log::info!("Somewhere we got null {:?}", node);
//     NodeResult::Val(MsVal::new(null, MsType::Native(MsNativeType::Void)))
// }
