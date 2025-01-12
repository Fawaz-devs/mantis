use crate::ms::MsContext;
use crate::native::instructions::NodeResult;
use crate::registries::functions::{FunctionType, MsDeclaredFunction};
use crate::registries::modules::MsResolved;
use crate::registries::types::{
    binary_cmp_op_to_condcode_intcc, MsNativeType, MsType, MsTypeId, MsTypeWithId,
};
use crate::registries::variable::{MsVal, MsVar};
use crate::registries::MsRegistryExt;
use crate::scope::{drop_scope, drop_variable};
use codegen::ir::Inst;
use cranelift::{codegen::Context, prelude::*};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::ObjectModule;
use linear_map::LinearMap;
use logos::Source;
use mantis_expression::node::BinaryOperation;
use mantis_expression::pratt::{Block, ConditionalBlock, FunctionDecl, Node, Rule, Term, WordSpan};
use mantis_expression::pratt::{IfElseChain, Statement, Type as MsTokenType};
use rand::Rng;
use std::fmt::Write;
use std::ops::Deref;
use std::rc::Rc;

pub enum FinalType {
    Name(Box<str>),
    Type(Type),
    CraneliftType(types::Type),
}

pub struct TraitFunctionFor<'a> {
    pub trait_name: &'a str,
    pub on_type: &'a MsTypeWithId,
}

pub fn compile_function(
    function: FunctionDecl,
    module: &mut ObjectModule,
    ctx: &mut Context,
    fbx: &mut FunctionBuilderContext,
    ms_ctx: &mut MsContext,
    trait_on_type: Option<TraitFunctionFor>,
    exporting_fn_name: Option<&str>,
) -> Rc<MsDeclaredFunction> {
    let name = function.name.word().unwrap();
    let mut linkage = Linkage::Preemptible;
    if function.is_extern {
        if matches!(function.block, Block::Empty) {
            linkage = Linkage::Import;
        } else {
            linkage = Linkage::Export;
        }
    }

    let self_type = if let Some(trait_on_type) = &trait_on_type {
        let ty = trait_on_type.on_type.clone();
        Some(ty)
    } else {
        None
    };

    let mut returns_struct_or_enum = false;
    let return_ty = {
        if !matches!(function.return_type, MsTokenType::Unknown) {
            let ty = ms_ctx
                .current_module
                .resolve(&function.return_type)
                .expect(&format!("invalid type_name"))
                .ty()
                .unwrap();

            match ty.ty {
                MsType::Native(nty) => {
                    ctx.func.signature.returns.push(nty.to_abi_param().unwrap());
                }
                MsType::Struct(struct_ty) => {
                    returns_struct_or_enum = true;
                    ctx.func.signature.params.push(struct_ty.to_abi_param());
                }
                MsType::Enum(enum_ty) => {
                    returns_struct_or_enum = true;
                    ctx.func.signature.params.push(enum_ty.to_abi_param());
                }
                _ => todo!(),
            };

            Some(ty.id)
        } else {
            None
        }
    };

    let mut fn_arguments = LinearMap::<Box<str>, MsTypeId>::with_capacity(function.arguments.len());
    for (k, v) in &function.arguments {
        let ty = ms_ctx.current_module.resolve(v).unwrap().ty().unwrap();

        ctx.func
            .signature
            .params
            .push(ty.ty.to_abi_param().unwrap());

        fn_arguments.insert(k.as_str().into(), ty.id);
    }

    let func_id = module
        .declare_function(
            exporting_fn_name.unwrap_or(name),
            linkage,
            &ctx.func.signature,
        )
        .unwrap();

    let declared_function = Rc::new(MsDeclaredFunction {
        func_id,
        arguments: fn_arguments,
        rets: return_ty,
        fn_type: if function.is_extern {
            FunctionType::Extern
        } else {
            FunctionType::Public
        },
    });
    ms_ctx
        .current_module
        .fn_registry
        .add_function(exporting_fn_name.unwrap_or(name), declared_function.clone());

    if let Some(tot) = &trait_on_type {
        ms_ctx.current_module.trait_registry.add_function(
            tot.trait_name,
            tot.on_type.id.clone(),
            name.into(),
            declared_function.clone(),
        );

        ms_ctx.current_module.type_fn_registry.add_function(
            tot.on_type.id,
            name,
            declared_function.clone(),
        );
    }

    if matches!(function.block, Block::Empty) {
        ctx.clear();
        return declared_function;
    }
    let mut f = FunctionBuilder::new(&mut ctx.func, fbx);
    ms_ctx.var_scopes.new_scope();

    let entry_block = f.create_block();
    f.append_block_params_for_function_params(entry_block);
    f.switch_to_block(entry_block);

    let block_params = f.block_params(entry_block).to_vec();

    let mut fn_args_iter = block_params.iter();
    if returns_struct_or_enum {
        let return_ty_id = return_ty.unwrap();
        let ty = ms_ctx
            .current_module
            .type_registry
            .get_from_type_id(return_ty_id)
            .expect(&format!("invalid type_name"));
        let var = ms_ctx.new_variable();
        f.declare_var(var, ty.to_cl_type().unwrap());
        let value = fn_args_iter.next().unwrap();
        f.def_var(var, *value);
        ms_ctx
            .var_scopes
            .add_variable("return", MsVar::new(return_ty_id, var, true, true));
    }

    for ((arg_name, arg_type), value) in declared_function.arguments.iter().zip(fn_args_iter) {
        let var = ms_ctx.new_variable();
        let ty = ms_ctx
            .current_module
            .type_registry
            .get_from_type_id(arg_type.clone())
            .expect("invalid type_name");

        f.declare_var(var, ty.to_cl_type().unwrap());
        f.def_var(var, *value);
        ms_ctx.var_scopes.add_variable(
            arg_name.deref(),
            MsVar::new(arg_type.clone(), var, true, true), // ignoring mutability
        );
    }

    {
        if compile_block(&function.block, module, &mut f, ms_ctx).is_none() {
            let scope = ms_ctx.var_scopes.exit_scope().unwrap();
            drop_scope(&scope, ms_ctx, &mut f, module);
            f.ins().return_(&[]);
        }
    }
    f.seal_block(entry_block);

    // let mut ir = String::new();
    // codegen::write_function(&mut ir, &f.func).unwrap();

    f.finalize();
    module.define_function(func_id, ctx).unwrap();
    ctx.clear();

    return declared_function;
}

pub fn compile_block(
    block: &Block,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> Option<Inst> {
    match block {
        Block::Closed(block) => {
            ms_ctx.var_scopes.new_scope();
            compile_block(&block, module, fbx, ms_ctx);
            let vars = ms_ctx.var_scopes.exit_scope().unwrap();
            drop_scope(&vars, ms_ctx, fbx, module);
        }
        Block::Statements(stmts) => return compile_statements(&stmts, module, fbx, ms_ctx),
        Block::Continuos(blocks) => {
            let mut last_inst = None;
            for block in blocks {
                last_inst = compile_block(block, module, fbx, ms_ctx);
            }

            return last_inst;
        }
        Block::IfElseChain(if_else_chain) => {
            compile_if_else_chain(&if_else_chain, fbx, module, ms_ctx);
        }
        Block::Loop(word_span, block) => {
            compile_loop(word_span.clone(), block, module, fbx, ms_ctx);
        }
        Block::Match(node, vec) => todo!("match cases unimplemented"),
        Block::Empty => {
            log::warn!("Empty block, doing nothing")
        }
    }

    return None;
}

pub fn rule_to_binary_operation(rule: Rule) -> Option<BinaryOperation> {
    let op = match rule {
        Rule::add => BinaryOperation::Add,
        Rule::sub => BinaryOperation::Sub,
        Rule::mul => BinaryOperation::Mult,
        Rule::div => BinaryOperation::Div,
        Rule::cast => BinaryOperation::Cast,
        Rule::gt_eq => BinaryOperation::GreaterThanOrEqualTo,
        Rule::ge => BinaryOperation::GreaterThan,
        Rule::le => BinaryOperation::LessThan,
        Rule::le_eq => BinaryOperation::LessThanOrEqualTo,
        Rule::eq => BinaryOperation::EqualTo,
        Rule::not_eq => BinaryOperation::NotEqualTo,
        Rule::assign => BinaryOperation::Assign,
        _ => return None,
    };
    Some(op)
}

pub fn compile_binary_operation(
    op: BinaryOperation,
    lhs: Value,
    rhs: Value,
    ty: MsNativeType,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> Value {
    match op {
        BinaryOperation::Add => ty.add(lhs, rhs, fbx),
        BinaryOperation::Sub => ty.sub(lhs, rhs, fbx),
        BinaryOperation::Div => ty.div(lhs, rhs, fbx),
        BinaryOperation::Mult => ty.mult(lhs, rhs, fbx),
        BinaryOperation::GreaterThan
        | BinaryOperation::GreaterThanOrEqualTo
        | BinaryOperation::EqualTo
        | BinaryOperation::NotEqualTo
        | BinaryOperation::LessThan
        | BinaryOperation::LessThanOrEqualTo => ty.compare(op, lhs, rhs, fbx),
        _ => unimplemented!("unhandled binary operation"),
    }
}

pub fn compile_assignment(
    lhs: &str,
    rhs: NodeResult,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) {
    let variable = ms_ctx
        .var_scopes
        .find_variable(lhs)
        .expect(&format!("undeclared variable {lhs}"));

    if !variable.is_mutable {
        panic!("{lhs} is marked immutable");
    }

    assert!(variable.ty_id == rhs.ty());

    let ty = ms_ctx
        .current_module
        .type_registry
        .get_from_type_id(variable.ty_id)
        .unwrap();
    match ty {
        MsType::Native(_) => {
            let value = rhs.value(fbx);
            fbx.def_var(variable.c_var, value);
        }
        MsType::Struct(sty) => {
            // TODO: drop the old struct
            drop_variable(variable, ms_ctx, fbx, module);

            let dest = variable.value(fbx);
            let src = rhs.value(fbx);
            sty.copy(dest, src, fbx, module, ms_ctx);
        }
        _ => todo!(),
    }
}

pub fn compile_assignment_on_pointers(
    lhs: NodeResult,
    rhs: NodeResult,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) {
    let ty = ms_ctx
        .current_module
        .type_registry
        .get_from_type_id(rhs.ty())
        .unwrap();
    match ty {
        MsType::Native(nty) => {
            let value = rhs.value(fbx);
            let ptr = lhs.value(fbx);
            fbx.ins().store(MemFlags::new(), value, ptr, 0);
        }
        MsType::Struct(sty) => {
            let dest = lhs.value(fbx);
            let src = rhs.value(fbx);
            sty.copy(dest, src, fbx, module, ms_ctx);
        }
        _ => todo!(),
    }
}

pub fn compile_cast(
    value: NodeResult,
    cast_to: MsTypeWithId,

    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> NodeResult {
    let ty = ms_ctx
        .current_module
        .type_registry
        .get_from_type_id(value.ty())
        .unwrap();

    match ty {
        MsType::Native(nty) => {
            return NodeResult::Val(MsVal::new(
                cast_to.id,
                nty.cast_to(value.value(fbx), &cast_to.ty, fbx),
            ))
        }
        _ => unimplemented!("only native types are castable"),
    }
}

pub fn compile_nested_struct_field_to_ptr(
    root_struct_ptr: Value,
    root_struct_ty: MsTypeId,
    child: &MsTokenType,
    ms_ctx: &mut MsContext,
    fbx: &mut FunctionBuilder,
) -> NodeResult {
    let Some(MsType::Struct(struct_ty)) = ms_ctx
        .current_module
        .type_registry
        .get_from_type_id(root_struct_ty)
    else {
        panic!("didn't find struct of type {}", root_struct_ty);
    };

    match child {
        MsTokenType::Word(field_name) => {
            // let ty = root_struct_ty.struct_ty().unwrap();
            let ty = struct_ty;
            let field = ty.get_field(&field_name).expect(&format!(
                "no field with name {} on {:?}",
                field_name.as_str(),
                root_struct_ty
            ));
            let value = fbx.ins().iadd_imm(root_struct_ptr, field.offset as i64);
            return NodeResult::Val(MsVal::new(field.ty, value));
        }
        MsTokenType::Nested(field_name, child) => {
            let ty = struct_ty;
            // let ty = root_struct_ty.struct_ty().unwrap();
            let field_name = field_name.word().unwrap();
            let field = ty.get_field(&field_name).expect(&format!(
                "no field with name {} on {:?}",
                field_name, root_struct_ty
            ));
            let value = fbx.ins().iadd_imm(root_struct_ptr, field.offset as i64);
            // let ty = ms_ctx
            //     .current_module
            //     .type_registry
            //     .get_from_type_id(field.ty)
            //     .unwrap();

            return compile_nested_struct_field_to_ptr(value, field.ty, child, ms_ctx, fbx);
        }
        _ => unreachable!(),
    }

    todo!()
}

pub fn compile_node(
    node: &Node,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> Option<NodeResult> {
    match node {
        Node::Binary(rule, lhs, rhs) => {
            if matches!(rule, Rule::cast) {
                let value = compile_node(lhs, module, fbx, ms_ctx).unwrap();
                let ty_name = match rhs.deref() {
                    Node::Term(Term::Type(term)) => term.word().unwrap(),
                    _ => unreachable!(),
                };
                let ty = ms_ctx
                    .current_module
                    .type_registry
                    .get_from_str(ty_name)
                    .expect(&format!("undefined type {}", ty_name));

                let value = compile_cast(value, ty.clone(), module, fbx, ms_ctx);

                return Some(value);
            } else if matches!(rule, Rule::assign) {
                // TODO: Check for enum unwrapping

                let rhs = compile_node(rhs, module, fbx, ms_ctx).unwrap();
                match lhs.deref() {
                    Node::Term(Term::Type(term)) => {
                        match term {
                            MsTokenType::Word(word_span) => {
                                let variable_name = term.word().unwrap();
                                compile_assignment(&variable_name, rhs, module, fbx, ms_ctx);
                            }
                            MsTokenType::Nested(root, child) => {
                                let variable_name = root.word().unwrap();
                                let var = ms_ctx.var_scopes.find_variable(variable_name).unwrap();
                                let ptr = var.value(fbx);
                                let final_ptr = compile_nested_struct_field_to_ptr(
                                    ptr, var.ty_id, child, ms_ctx, fbx,
                                );

                                let ty = ms_ctx
                                    .current_module
                                    .type_registry
                                    .get_from_type_id(final_ptr.ty())
                                    .unwrap();
                                // final_ptr.ty();

                                match ty {
                                    MsType::Struct(_) => {
                                        compile_assignment_on_pointers(
                                            final_ptr, rhs, module, fbx, ms_ctx,
                                        );
                                    }
                                    MsType::Enum(enum_ty) => {
                                        let op = binary_cmp_op_to_condcode_intcc(
                                            BinaryOperation::EqualTo,
                                            false,
                                        );
                                        let tag = enum_ty.get_tag(final_ptr.value(fbx), fbx);
                                        let enum_variant_name: &str =
                                            todo!("parse enum variant name for matching");
                                        let expected_tag = enum_ty
                                            .get_tag_index(enum_variant_name)
                                            .expect(&format!(
                                                "undefined enum variant {}",
                                                enum_variant_name
                                            ));

                                        let value =
                                            fbx.ins().icmp_imm(op, tag, expected_tag as i64);

                                        let ty_id = ms_ctx
                                            .current_module
                                            .resolve_from_str("i8")
                                            .unwrap()
                                            .ty()
                                            .unwrap();

                                        return Some(NodeResult::Val(MsVal::new(ty_id.id, value)));
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        };
                    }
                    _ => {
                        let lhs = compile_node(lhs, module, fbx, ms_ctx).unwrap();
                        compile_assignment_on_pointers(lhs, rhs, module, fbx, ms_ctx);
                    }
                };
                return None;
            } else {
                let lhs = compile_node(lhs, module, fbx, ms_ctx).unwrap();
                let rhs = compile_node(rhs, module, fbx, ms_ctx).unwrap();
                let op = rule_to_binary_operation(*rule)
                    .expect(&format!("unhandled rule to binary operation {:?}", rule));

                let ty = ms_ctx
                    .current_module
                    .type_registry
                    .get_from_type_id(lhs.ty())
                    .unwrap();

                let nty = if let MsType::Native(nty) = ty {
                    nty
                } else {
                    MsNativeType::I64
                };
                let lval = lhs.value(fbx);
                let rval = rhs.value(fbx);
                let value = compile_binary_operation(op, lval, rval, nty, module, fbx, ms_ctx);

                let value = NodeResult::Val(MsVal::new(lhs.ty(), value));

                return Some(value);
            }
        }
        Node::Unary(rule, node) => {
            if matches!(rule, Rule::deref) {
                let node = compile_node(node, module, fbx, ms_ctx).unwrap();
                let ptr_value = node.value(fbx);

                let ty = ms_ctx
                    .current_module
                    .type_registry
                    .get_from_type_id(node.ty())
                    .unwrap();

                match ty {
                    MsType::Native(nty) => {
                        let val = fbx.ins().load(
                            nty.to_cl_type().unwrap(),
                            MemFlags::new(),
                            ptr_value,
                            0,
                        );

                        return Some(NodeResult::Val(MsVal::new(node.ty(), val)));
                    }
                    MsType::Struct(struct_ty) => {
                        return Some(NodeResult::Val(MsVal::new(node.ty(), ptr_value)));
                        // todo!()
                    }
                    _ => todo!(),
                };
            }

            todo!("unary operation not implemented")
        }
        Node::Expr(node) => {
            return compile_node(node, module, fbx, ms_ctx);
        }
        Node::FnCall(node, args) => {
            let Node::Term(Term::Type(fn_name)) = node.deref() else {
                unreachable!("other node types not supported FnCall");
            };

            let mut method_on_variable: Option<MsVar> = None;
            let func = match fn_name.clone() {
                MsTokenType::Nested(root, child) => {
                    let either_var_or_ty_name = root.word().unwrap();
                    if let Some(var) = ms_ctx
                        .var_scopes
                        .find_variable(either_var_or_ty_name)
                        .cloned()
                    {
                        let method_name = child.word().unwrap();

                        let reg = ms_ctx
                            .current_module
                            .type_fn_registry
                            .map
                            .get(&var.ty_id)
                            .unwrap();
                        let func = reg.registry.get(method_name).unwrap();
                        method_on_variable = Some(var.clone());
                        func.clone()
                    } else {
                        let Some(MsResolved::Function(func)) =
                            ms_ctx.current_module.resolve(fn_name)
                        else {
                            panic!("couldn't find function {:?}", fn_name);
                        };

                        func
                    }
                }
                _ => match ms_ctx.current_module.resolve(fn_name).unwrap() {
                    MsResolved::Function(func) => func,
                    MsResolved::EnumUnwrap(enum_ty, variant_name) => {
                        let MsType::Enum(enum_ty) = enum_ty.ty else {
                            unreachable!()
                        };
                        assert!(args.len() == 1);
                        let variant_type = enum_ty.get_inner_ty(&variant_name).unwrap();
                        let arg = args.first().unwrap();
                        if let Node::Term(Term::Type(MsTokenType::Word(var_name))) = arg {
                            let c_var = ms_ctx.new_variable();
                            let var = MsVar::new(variant_type.id, c_var, true, false);
                            todo!("either move this enum unwrapping to assignment handling, since it requires left and right values");
                            // let ptr = enum_ty.get_inner_ptr(, )
                            // fbx.def_var(var, );
                            // ms_ctx.var_scopes.add_variable(var_name.as_str(), var);
                        } else {
                            unreachable!();
                        }

                        return None;
                    }
                    _ => unreachable!(),
                },
            };

            let mut call_arg_values = Vec::with_capacity(args.len());
            if let Some(fn_ret_ty) = func.rets {
                let return_ty = ms_ctx
                    .current_module
                    .type_registry
                    .get_from_type_id(fn_ret_ty)
                    .unwrap();

                let mut returns_a_struct_ptr: Option<Value> = None;
                match return_ty {
                    MsType::Native(nty) => {}
                    MsType::Struct(sty) => {
                        let stackslot = fbx.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            sty.size() as u32,
                        ));
                        let ptr = fbx.ins().stack_addr(types::I64, stackslot, 0);
                        call_arg_values.push(ptr);

                        returns_a_struct_ptr = Some(ptr);
                    }

                    MsType::Enum(ety) => {
                        let stackslot = fbx.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            ety.size() as u32,
                        ));
                        let ptr = fbx.ins().stack_addr(types::I64, stackslot, 0);
                        call_arg_values.push(ptr);
                        returns_a_struct_ptr = Some(ptr);
                    }
                    _ => todo!(),
                }

                if let Some(var) = method_on_variable {
                    call_arg_values.push(var.value(fbx));
                }

                for arg in args {
                    let val = compile_node(arg, module, fbx, ms_ctx).unwrap().value(fbx);
                    call_arg_values.push(val);
                }

                let func_ref = module.declare_func_in_func(func.func_id, fbx.func);
                log::info!("calling a function {:?}", node);
                let inst = fbx.ins().call(func_ref, &call_arg_values);
                let result = fbx.inst_results(inst);

                if !result.is_empty() {
                    let return_value = result[0];
                    return Some(NodeResult::Val(MsVal::new(fn_ret_ty, return_value)));
                } else {
                    if let Some(ptr) = returns_a_struct_ptr {
                        return Some(NodeResult::Val(MsVal::new(fn_ret_ty, ptr)));
                    }
                }
            } else {
                if let Some(var) = method_on_variable {
                    call_arg_values.push(var.value(fbx));
                }

                for arg in args {
                    let val = compile_node(arg, module, fbx, ms_ctx).unwrap().value(fbx);
                    call_arg_values.push(val);
                }

                let func_ref = module.declare_func_in_func(func.func_id, fbx.func);
                log::info!("calling a function {:?}", node);
                let _inst = fbx.ins().call(func_ref, &call_arg_values);
            }
        }
        Node::Term(term) => {
            match term {
                Term::Array(arr) => {
                    let mut arr_ty: Option<MsTypeId> = None;
                    let mut nodes = Vec::new();
                    for element in arr {
                        let node = compile_node(node, module, fbx, ms_ctx).unwrap();
                        if let Some(ty) = arr_ty.clone() {
                            assert!(ty == node.ty());
                        } else {
                            arr_ty = Some(node.ty());
                        }
                        nodes.push(node);
                    }
                    let arr_inner_ty = ms_ctx
                        .current_module
                        .type_registry
                        .get_from_type_id(arr_ty.unwrap())
                        .unwrap();
                    let arr_stack_size = arr_inner_ty.size() * nodes.len() + 8; // 8 for length
                    let stackslot = fbx.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        arr_stack_size as u32,
                    ));

                    todo!();
                }
                Term::Function(function_decl) => todo!(),
                Term::String(word_span) => {
                    let string_literal = word_span.as_str();
                    let string_literal = &string_literal[1..string_literal.len() - 1];
                    let mut s = String::new();
                    let mut is_escaping = false;
                    for c in string_literal.chars() {
                        if c == '\\' {
                            is_escaping = true;
                        } else {
                            if is_escaping {
                                let c = match c {
                                    'n' => '\n',
                                    't' => '\t',
                                    'r' => '\r',
                                    '\\' => '\\',
                                    _ => panic!("unhandled escape sequence"),
                                };
                                s.push(c);
                                is_escaping = false;
                            } else {
                                s.push(c);
                            }
                        }
                    }

                    let content = s;

                    let mut data_name = String::with_capacity(32);
                    {
                        let range = word_span.range();
                        random_string_into(20, &mut data_name);
                        write!(&mut data_name, "_{}_{}", range.start, range.end).unwrap();
                    }

                    let ty = ms_ctx
                        .current_module
                        .type_registry
                        .get_from_str("StrSlice")
                        .unwrap();
                    let MsType::Struct(sty) = ty.ty else {
                        panic!("expected struct");
                    };
                    // str len: 8 + ptr: 8
                    let stack_slot = fbx.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        sty.size() as u32,
                    ));
                    let size_of_data = fbx.ins().iconst(types::I64, content.len() as i64);

                    let ty_i64 = ms_ctx
                        .current_module
                        .type_registry
                        .get_from_str("i64")
                        .unwrap();

                    let struct_ptr = fbx.ins().stack_addr(types::I64, stack_slot, 0);

                    let data_ptr = if content.is_empty() {
                        fbx.ins().iconst(ty_i64.ty.to_cl_type().unwrap(), 0)
                    } else {
                        let data_id = module
                            .declare_data(&data_name, Linkage::Preemptible, false, false)
                            .unwrap();
                        let mut data_desc = DataDescription::new();
                        data_desc.define(content.as_bytes().into());
                        module.define_data(data_id, &data_desc).unwrap();
                        let gl_value = module.declare_data_in_func(data_id, fbx.func);
                        fbx.ins()
                            .global_value(ty_i64.ty.to_cl_type().unwrap(), gl_value)
                    };

                    sty.set_field(
                        &MsVal::new(ty_i64.id, struct_ptr),
                        "len",
                        &MsVal::new(ty_i64.id, size_of_data),
                        ms_ctx,
                        fbx,
                        module,
                    );
                    sty.set_field(
                        &MsVal::new(ty_i64.id, struct_ptr),
                        "pointer",
                        &MsVal::new(ty_i64.id, data_ptr),
                        ms_ctx,
                        fbx,
                        module,
                    );

                    let mut val = MsVal::new(ty.id, struct_ptr);

                    return Some(NodeResult::Val(val));
                }
                Term::Type(type_name) => match type_name {
                    MsTokenType::Word(word_span) => {
                        let var_name = word_span.as_str();
                        if let Some(var) = ms_ctx.var_scopes.find_variable(&var_name) {
                            return Some(NodeResult::Var(var.clone()));
                        } else {
                            panic!(
                                "undefined {} word or type name in current scope\n{}",
                                var_name,
                                word_span.highlight()
                            );
                        }
                    }
                    MsTokenType::Nested(root, child) => {
                        let var_name = root.word().unwrap();
                        if let Some(var) = ms_ctx.var_scopes.find_variable(&var_name).cloned() {
                            let var = NodeResult::Var(var);
                            return Some(compile_nested_struct_access(
                                var, child, ms_ctx, fbx, module,
                            ));
                        } else {
                            log::warn!(
                                "Didn't find a variable with {}, should resolve types",
                                var_name
                            );
                            // compile_nested_struct_field_to_ptr(, , , , )

                            panic!(
                                "undefined {} word or type name in current scope\n{}",
                                var_name,
                                root.word_highlight().unwrap()
                            );
                        }
                    }
                    _ => todo!(),
                },
                Term::I64(val) => {
                    let ty = ms_ctx
                        .current_module
                        .type_registry
                        .get_from_str("i64")
                        .unwrap();

                    let cty = ty.ty.to_cl_type().unwrap();
                    let val = fbx.ins().iconst(cty, *val);
                    return Some(NodeResult::Val(MsVal::new(ty.id, val)));
                }
                Term::F64(val) => {
                    let ty = ms_ctx
                        .current_module
                        .type_registry
                        .get_from_str("f64")
                        .unwrap();
                    let cty = ty.ty.to_cl_type().unwrap();
                    let val = fbx.ins().f64const(*val);
                    return Some(NodeResult::Val(MsVal::new(ty.id, val)));
                }
                Term::Char(c) => {
                    let c = *c as i32; // utf8
                    let ty = ms_ctx
                        .current_module
                        .type_registry
                        .get_from_str("char")
                        .unwrap();
                    let cty = ty.ty.to_cl_type().unwrap();
                    let val = fbx.ins().iconst(cty, c as i64);
                    return Some(NodeResult::Val(MsVal::new(ty.id, val)));
                }
                Term::Struct(ty, linear_map) => {
                    let ty_name = ty.word().unwrap();
                    let ty = ms_ctx
                        .current_module
                        .type_registry
                        .get_from_str(ty_name)
                        .expect(&format!(
                            "couldn't find type_name {ty_name}\n{}",
                            ty.word_highlight().unwrap()
                        ));

                    let MsType::Struct(struct_type) = ty.ty else {
                        panic!("undefined struct {}", ty_name);
                    };

                    let stack_slot = fbx.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        struct_type.size() as u32,
                    ));
                    let ptr = fbx.ins().stack_addr(types::I64, stack_slot, 0);

                    let ptr = MsVal::new(ty.id, ptr);
                    for (k, v) in linear_map {
                        let val = compile_node(v, module, fbx, ms_ctx);
                        let val = val.unwrap();
                        let val = val.to_ms_val(fbx);
                        struct_type.set_field(&ptr, k, &val, ms_ctx, fbx, module);
                    }

                    return Some(NodeResult::Val(ptr));
                }
            }
        }
        Node::None => {
            fbx.ins().nop();
        }
    }

    return None;
}

pub fn compile_nested_struct_access(
    root: NodeResult,
    child: &MsTokenType,
    ms_ctx: &mut MsContext,
    fbx: &mut FunctionBuilder,
    module: &mut ObjectModule,
) -> NodeResult {
    // let var_name = root.word().unwrap();
    // if let Some(var) = ms_ctx.var_scopes.find_variable(&var_name).cloned() {

    let var = root;
    let var_ty = ms_ctx
        .current_module
        .type_registry
        .get_from_type_id(var.ty())
        .unwrap();

    match var_ty {
        MsType::Native(ms_native_type) => {
            return var;
        }
        MsType::Struct(struct_ty) => {
            match child {
                MsTokenType::Word(field_name) => {
                    let field_name = field_name.as_str();
                    let field = struct_ty
                        .get_field(field_name)
                        .expect(&format!("unknown field in struct {}", field_name));

                    let ptr = var.value(fbx);

                    let ptr = MsVal::new(var.ty(), ptr);
                    let val = struct_ty.get_data(ptr, field_name, ms_ctx, fbx, module);
                    return NodeResult::Val(val);
                }
                MsTokenType::Nested(field_name, child) => {
                    let field_name = field_name.word().unwrap();
                    let field = struct_ty
                        .get_field(field_name)
                        .expect(&format!("unknown field in struct {}", field_name));

                    let ptr = var.value(fbx);

                    let ptr = MsVal::new(var.ty(), ptr);
                    let val = struct_ty.get_data(ptr, field_name, ms_ctx, fbx, module);

                    let child_ptr = NodeResult::Val(val);

                    return compile_nested_struct_access(child_ptr, child, ms_ctx, fbx, module);
                }
                _ => unreachable!(),
            };
        }
        _ => todo!(),
    };
}

pub fn implicit_cast(
    var: NodeResult,
    ty: MsTypeWithId,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> NodeResult {
    if var.ty() == ty.id {
        return var;
    }

    todo!()
}

pub fn compile_statements(
    statements: &[Statement],
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> Option<Inst> {
    for stmt in statements {
        match stmt {
            Statement::Let(word_span, node, expected_type, is_mutable) => {
                let var_name = word_span.as_str();
                let mut value = compile_node(node, module, fbx, ms_ctx).unwrap();

                if !matches!(expected_type, MsTokenType::Unknown) {
                    let type_name = expected_type.word().unwrap();
                    let ty = ms_ctx
                        .current_module
                        .type_registry
                        .get_from_str(type_name)
                        .unwrap()
                        .clone();
                    value = implicit_cast(value, ty, module, fbx, ms_ctx);
                }

                let node_value = value;
                let ty = node_value.ty();

                let ty = ms_ctx
                    .current_module
                    .type_registry
                    .get_from_type_id(ty)
                    .unwrap();

                let value = node_value.value(fbx);
                let variable = ms_ctx.new_variable();
                fbx.declare_var(variable, ty.to_cl_type().unwrap());
                fbx.def_var(variable, value);
                let mut variable = MsVar::new(node_value.ty(), variable, *is_mutable, false);
                // variable.mark_mutability(*is_mutable);
                if let Some(old_variable) = ms_ctx.var_scopes.add_variable(var_name, variable) {
                    drop_variable(&old_variable, ms_ctx, fbx, module);
                }
            }
            Statement::Return(node) => {
                let ret_inst = if let Some(var) = compile_node(node, module, fbx, ms_ctx) {
                    let ty = ms_ctx
                        .current_module
                        .type_registry
                        .get_from_type_id(var.ty())
                        .unwrap();

                    match ty {
                        MsType::Native(nty) => {
                            let val = var.value(fbx);
                            fbx.ins().return_(&[val])
                        }
                        MsType::Struct(sty) => {
                            let src = var.value(fbx);

                            let dest = ms_ctx.var_scopes.find_variable("return").unwrap().c_var;
                            let dest = fbx.use_var(dest);

                            sty.copy(dest, src, fbx, module, ms_ctx);

                            fbx.ins().return_(&[])
                        }
                        _ => todo!(),
                    }
                } else {
                    fbx.ins().return_(&[])
                };

                return Some(ret_inst);
            }
            Statement::Break(word_span) => {
                // log::warn!(
                //     "dropping variables is unhandled when breaking out of loop {:?}",
                //     word_span
                // );

                let name = word_span.as_ref().map(|x| x.as_str());

                return Some(ms_ctx.loop_scopes.break_out_of_loop(name, fbx));
            }
            Statement::Continue(word_span) => {
                // log::warn!(
                //     "dropping variables is unhandled when continuing the loop {:?}",
                //     word_span
                // );

                let name = word_span.as_ref().map(|x| x.as_str());
                return Some(ms_ctx.loop_scopes.continue_loop(name, fbx));
            }
            Statement::Expr(node) => {
                compile_node(node, module, fbx, ms_ctx);
            }
        }
    }

    return None;
}

pub fn compile_loop(
    loop_name: Option<WordSpan>,
    loop_block: &Block,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) {
    let loop_name = loop_name.as_ref().map(|x| x.as_str());
    ms_ctx.var_scopes.new_scope();
    ms_ctx.new_loop_scope(loop_name.map(|x| x.into()), fbx);

    compile_block(loop_block, module, fbx, ms_ctx);

    ms_ctx.loop_scopes.end_loop(loop_name, fbx);
    let scope = ms_ctx.var_scopes.exit_scope().unwrap();
    drop_scope(&scope, &ms_ctx, fbx, module);
}

pub struct IfElseChainBuilder {
    else_block: Option<cranelift::prelude::Block>,
    end_block: cranelift::prelude::Block,
}

impl IfElseChainBuilder {
    pub fn new_block(
        block: &ConditionalBlock,
        fbx: &mut FunctionBuilder,
        module: &mut ObjectModule,
        ms_ctx: &mut MsContext,
    ) -> Self {
        let if_block = fbx.create_block();
        let else_block = fbx.create_block();
        let end_block = fbx.create_block();

        ms_ctx.var_scopes.new_scope();

        let value = compile_node(&block.condition, module, fbx, ms_ctx).unwrap();
        let value = value.value(fbx);
        fbx.ins().brif(value, if_block, &[], else_block, &[]);
        fbx.switch_to_block(if_block);

        let inst = compile_block(&block.block, module, fbx, ms_ctx);
        let scope = ms_ctx.var_scopes.exit_scope().unwrap();
        drop_scope(&scope, ms_ctx, fbx, module);
        if inst.is_none() {
            fbx.ins().jump(end_block, &[]);
        }

        fbx.seal_block(if_block);

        Self {
            else_block: Some(else_block),
            end_block,
        }
    }

    pub fn elseif_block(
        &mut self,
        block: &ConditionalBlock,
        fbx: &mut FunctionBuilder,
        module: &mut ObjectModule,
        ms_ctx: &mut MsContext,
    ) {
        let if_block = fbx.create_block();
        let else_block = fbx.create_block();
        let previous_else_block = self.else_block.replace(else_block).unwrap();
        let end_block = self.end_block;
        fbx.switch_to_block(previous_else_block);

        ms_ctx.var_scopes.new_scope();

        let value = compile_node(&block.condition, module, fbx, ms_ctx).unwrap();
        let value = value.value(fbx);
        fbx.ins().brif(value, if_block, &[], else_block, &[]);
        fbx.seal_block(previous_else_block);

        fbx.switch_to_block(if_block);

        let inst = compile_block(&block.block, module, fbx, ms_ctx);
        let scope = ms_ctx.var_scopes.exit_scope().unwrap();
        drop_scope(&scope, ms_ctx, fbx, module);
        if inst.is_none() {
            fbx.ins().jump(end_block, &[]);
        }
        fbx.seal_block(if_block);
    }

    pub fn else_block(
        &mut self,
        block: &Block,
        fbx: &mut FunctionBuilder,
        module: &mut ObjectModule,
        ms_ctx: &mut MsContext,
    ) {
        let else_block = self.else_block.take().unwrap();
        fbx.switch_to_block(else_block);
        ms_ctx.var_scopes.new_scope();
        let inst = compile_block(&block, module, fbx, ms_ctx);
        let scope = ms_ctx.var_scopes.exit_scope().unwrap();
        drop_scope(&scope, ms_ctx, fbx, module);
        if inst.is_none() {
            fbx.ins().jump(self.end_block, &[]);
        }
        fbx.seal_block(else_block);
    }

    pub fn end(
        &mut self,
        fbx: &mut FunctionBuilder,
        module: &mut ObjectModule,
        ms_ctx: &mut MsContext,
    ) {
        if self.else_block.is_some() {
            self.else_block(&Block::Empty, fbx, module, ms_ctx);
        }
        fbx.switch_to_block(self.end_block);
        fbx.seal_block(self.end_block);
    }
}

pub fn compile_if_else_chain(
    if_else_chain: &IfElseChain,
    fbx: &mut FunctionBuilder,
    module: &mut ObjectModule,
    ms_ctx: &mut MsContext,
) {
    let mut builder = IfElseChainBuilder::new_block(&if_else_chain.if_block, fbx, module, ms_ctx);

    for elseifblocks in if_else_chain.elif_blocks.iter() {
        builder.elseif_block(&elseifblocks, fbx, module, ms_ctx);
    }

    if let Some(elseblock) = &if_else_chain.else_block {
        builder.else_block(&elseblock, fbx, module, ms_ctx);
    }

    builder.end(fbx, module, ms_ctx);
}

pub fn random_readable_char(mut rng: impl rand::Rng) -> char {
    let val = rng.gen_range(0..=10);
    match val {
        0..3 => rng.gen_range('a'..='z'),
        3..6 => rng.gen_range('A'..='Z'),
        6..9 => rng.gen_range('0'..='9'),
        _ => '_',
    }
}

pub fn random_string(len: usize) -> String {
    let mut s = String::with_capacity(len);
    random_string_into(len, &mut s);
    s
}

pub fn random_string_into(len: usize, mut w: impl Write) {
    let mut rng = rand::thread_rng();
    w.write_char(rng.gen_range('a'..='z'));
    for _ in 1..len {
        let c = random_readable_char(&mut rng);
        w.write_char(c).unwrap();
    }
}
