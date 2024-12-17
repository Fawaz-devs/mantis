use cranelift::prelude::*;
use cranelift_module::{DataDescription, FuncOrDataId, Linkage, Module};
use cranelift_object::ObjectModule;
use mantis_expression::node::BinaryOperation;
use mantis_tokens::MantisLexerTokens;

use crate::{
    frontend::tokens::{MsContext, MsNode},
    registries::{
        types::{MsNativeType, MsType},
        variable::{MsVal, MsVar},
        MsRegistryExt,
    },
};

#[inline]
fn i32_to_i64(a: Value, fbx: &mut FunctionBuilder) -> Value {
    fbx.ins().sextend(types::I64, a)
}

fn i64_to_i32(a: Value, fbx: &mut FunctionBuilder) -> Value {
    fbx.ins().ireduce(types::I32, a)
}

fn f32_to_f64(a: Value, fbx: &mut FunctionBuilder) -> Value {
    fbx.ins().fpromote(types::F64, a)
}

fn f64_to_f32(a: Value, fbx: &mut FunctionBuilder) -> Value {
    fbx.ins().fdemote(types::F32, a)
}

fn float_to_i32(a: Value, fbx: &mut FunctionBuilder) -> Value {
    fbx.ins().fcvt_to_sint(types::I32, a)
}

fn float_to_i64(a: Value, fbx: &mut FunctionBuilder) -> Value {
    fbx.ins().fcvt_to_sint(types::I64, a)
}

fn translate_binary_op(
    op: BinaryOperation,
    lhs: &mantis_expression::node::Node,
    rhs: &mantis_expression::node::Node,

    ms_ctx: &mut MsContext,
    fbx: &mut FunctionBuilder<'_>,
    module: &mut ObjectModule,
) -> MsVal {
    match op {
        BinaryOperation::Add => {
            let lhs = translate_node(lhs, ms_ctx, fbx, module);
            let rhs = translate_node(rhs, ms_ctx, fbx, module);
            if !lhs.ty().equal(&rhs.ty()) {
                panic!(
                    "binary operation on non similar types {:?} != {:?}",
                    lhs.ty(),
                    rhs.ty()
                );
            }

            match lhs.ty() {
                MsType::Native(nty) => {
                    return MsVal::new(
                        nty.add(lhs.value(fbx), rhs.value(fbx), fbx),
                        lhs.ty().clone(),
                    )
                }
                _ => todo!(),
            };
        }
        BinaryOperation::Sub => {
            let lhs = translate_node(lhs, ms_ctx, fbx, module);
            let rhs = translate_node(rhs, ms_ctx, fbx, module);
            if !lhs.ty().equal(&rhs.ty()) {
                panic!(
                    "binary operation on non similar types {:?} != {:?}",
                    lhs.ty(),
                    rhs.ty()
                );
            }

            match lhs.ty() {
                MsType::Native(nty) => {
                    return MsVal::new(
                        nty.sub(lhs.value(fbx), rhs.value(fbx), fbx),
                        lhs.ty().clone(),
                    )
                }
                _ => todo!(),
            };
        }
        BinaryOperation::Div => {
            let lhs = translate_node(lhs, ms_ctx, fbx, module);
            let rhs = translate_node(rhs, ms_ctx, fbx, module);
            if !lhs.ty().equal(&rhs.ty()) {
                panic!(
                    "binary operation on non similar types {:?} != {:?}",
                    lhs.ty(),
                    rhs.ty()
                );
            }

            match lhs.ty() {
                MsType::Native(nty) => {
                    return MsVal::new(
                        nty.div(lhs.value(fbx), rhs.value(fbx), fbx),
                        lhs.ty().clone(),
                    )
                }
                _ => todo!(),
            };
        }
        BinaryOperation::Mult => {
            let lhs = translate_node(lhs, ms_ctx, fbx, module);
            let rhs = translate_node(rhs, ms_ctx, fbx, module);
            if !lhs.ty().equal(&rhs.ty()) {
                panic!(
                    "binary operation on non similar types {:?} != {:?}",
                    lhs.ty(),
                    rhs.ty()
                );
            }

            match lhs.ty() {
                MsType::Native(nty) => {
                    return MsVal::new(
                        nty.mult(lhs.value(fbx), rhs.value(fbx), fbx),
                        lhs.ty().clone(),
                    )
                }
                _ => todo!(),
            };
        }

        BinaryOperation::GreaterThan
        | BinaryOperation::GreaterThanOrEqualTo
        | BinaryOperation::EqualTo
        | BinaryOperation::NotEqualTo
        | BinaryOperation::LessThan
        | BinaryOperation::LessThanOrEqualTo => {
            let lhs = translate_node(lhs, ms_ctx, fbx, module);
            let rhs = translate_node(rhs, ms_ctx, fbx, module);
            if !lhs.ty().equal(&rhs.ty()) {
                panic!(
                    "binary operation on non similar types {:?} != {:?}",
                    lhs.ty(),
                    rhs.ty()
                );
            }

            match lhs.ty() {
                MsType::Native(nty) => {
                    return MsVal::new(
                        nty.compare(op, lhs.value(fbx), rhs.value(fbx), fbx),
                        lhs.ty().clone(),
                    )
                }
                _ => todo!(),
            };
        }

        BinaryOperation::Assign => {
            let l = translate_node(lhs, ms_ctx, fbx, module);
            let r = translate_node(rhs, ms_ctx, fbx, module);
            if !l.ty().equal(&r.ty()) {
                panic!(
                    "binary operation on non similar types {:?} of ty {:?} != {:?} of ty {:?}",
                    lhs, l, rhs, r,
                );
            }

            match l {
                Either::Left(_) => panic!("Can't assign to Value {:?} of type {:?}", lhs, l),
                Either::Right(var) => match var.ty {
                    MsType::Native(nty) => {
                        let val = r.value(fbx);
                        fbx.def_var(var.c_var, val);

                        return MsVal::new(val, var.ty);
                    }
                    _ => todo!(),
                },
            }
        }

        BinaryOperation::Cast => {
            let MsNode::Var(MantisLexerTokens::Word(type_name)) = rhs else {
                panic!("RHS is not type");
            };
            let l = translate_node(lhs, ms_ctx, fbx, module);

            let r = ms_ctx
                .type_registry
                .get(&type_name)
                .expect("undefined type name");

            match l.ty() {
                MsType::Native(nty) => {
                    return MsVal::new(nty.cast_to(l.value(fbx), r, fbx), r.clone())
                }
                _ => todo!(),
            }
        }
        BinaryOperation::Call => {
            let MsNode::Var(MantisLexerTokens::Word(fn_name)) = lhs else {
                panic!("No functoin name as token for call expr");
            };

            let MsNode::Tuple(args) = rhs else {
                panic!("function call expects arguments");
            };

            let Some(FuncOrDataId::Func(fn_id)) = module.get_name(&fn_name) else {
                panic!("no function with that name is declared");
            };

            let fn_ref = module.declare_func_in_func(fn_id, fbx.func);

            let values = args
                .iter()
                .map(|x| translate_node(x, ms_ctx, fbx, module).value(fbx))
                .collect::<Vec<_>>();

            let inst = fbx.ins().call(fn_ref, &values);
            let results = fbx.inst_results(inst);

            if !results.is_empty() {
                let value = results[0];

                let signature = ms_ctx
                    .fn_registry
                    .get(&fn_name)
                    .expect("Undeclared function");

                log::info!("Calling {} with signature {:?}", fn_name, signature);
                return MsVal::new(value, signature.rets.clone());
            }
        }
        BinaryOperation::Access => todo!(),
        BinaryOperation::Empty => todo!(),
    }

    todo!("{:?} {:?} {:?}", op, lhs, rhs)
}

#[derive(Debug, Clone)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl Either<MsVal, MsVar> {
    pub fn value(&self, fbx: &mut FunctionBuilder) -> Value {
        match self {
            Either::Left(val) => val.value,
            Either::Right(var) => fbx.use_var(var.c_var),
        }
    }

    pub fn ty(&self) -> &MsType {
        match self {
            Either::Left(val) => &val.ty,
            Either::Right(var) => &var.ty,
        }
    }
}

pub fn translate_node(
    node: &MsNode,
    ms_ctx: &mut MsContext,
    fbx: &mut FunctionBuilder<'_>,
    module: &mut ObjectModule,
) -> Either<MsVal, MsVar> {
    use mantis_expression::node::Node;

    match node {
        Node::Binary(op, lhs, rhs) => {
            return Either::Left(translate_binary_op(
                op.clone(),
                lhs.as_ref(),
                rhs.as_ref(),
                ms_ctx,
                fbx,
                module,
            ));
        }
        Node::Var(var_token) => {
            match var_token {
                MantisLexerTokens::Word(var_name) => {
                    let var = ms_ctx
                        .scopes
                        .get_variable(var_name)
                        .expect("Undeclared variable");

                    return Either::Right(MsVar::new(var.ty.clone(), var.c_var));
                }
                MantisLexerTokens::Integer(int) => {
                    let val = fbx.ins().iconst(types::I64, *int);
                    let ty = MsType::Native(MsNativeType::I64);
                    return Either::Left(MsVal::new(val, ty));
                }

                MantisLexerTokens::Float(float) => {
                    let val = fbx.ins().f64const(*float);
                    let ty = MsType::Native(MsNativeType::F64);
                    return Either::Left(MsVal::new(val, ty));
                }

                MantisLexerTokens::String(s) => {
                    if let Some(FuncOrDataId::Data(data_id)) = module.get_name(s) {
                    } else {
                        let data_id = module
                            .declare_data(s, Linkage::Local, false, false)
                            .unwrap();
                        let mut data_description = DataDescription::new();
                        data_description.define(s.as_bytes().into());
                        module.define_data(data_id, &data_description);

                        let gl_value = module.declare_data_in_func(data_id, fbx.func);
                        let val = fbx.ins().global_value(types::I64, gl_value);

                        let st = ms_ctx.type_registry.get("array").unwrap();
                    }
                }
                _ => panic!("Unsupported variable token {:?}", var_token),
            };
        }
        Node::Expr(inner_node) => return translate_node(inner_node.as_ref(), ms_ctx, fbx, module),
        _ => {}
    };

    let null = fbx.ins().null(types::I32);
    log::info!("Somewhere we got null {:?}", node);
    Either::Left(MsVal::new(null, MsType::Native(MsNativeType::Void)))
}
