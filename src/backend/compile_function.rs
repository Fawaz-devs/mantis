use crate::frontend::tokens::MsContext;
use crate::native::instructions::NodeResult;
use crate::registries::functions::{FunctionType, MsDeclaredFunction};
use crate::registries::types::{MsNativeType, MsType};
use crate::registries::variable::{MsVal, MsVar};
use crate::registries::MsRegistryExt;
use crate::scope::{drop_scope, drop_variable};
use codegen::ir::Inst;
use cranelift::{codegen::Context, prelude::*};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::ObjectModule;
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

pub fn resolve_typename(ty: &MsTokenType) -> Box<str> {
    match ty {
        MsTokenType::Enum { fields } => todo!(),
        MsTokenType::Struct { fields } => todo!(),
        MsTokenType::WithGenerics(_, vec) => todo!(),
        MsTokenType::Nested(_, _) => todo!(),
        MsTokenType::Unknown => "unknown".into(),
        MsTokenType::Word(word_span) => word_span.as_str().into(),
        MsTokenType::Ref(ty, mutable) => {
            let mut s = String::with_capacity(512);
            s.push('@');
            if *mutable {
                s.push_str(" mut ");
            }
            s.push_str(&resolve_typename(ty));

            s.into()
        }
    }
}

// pub fn type_name_into_cranelift_type(ty_name: &str, ctx: &MsContext) -> Option<types::Type> {
//     todo!()
// }

pub fn compile_function(
    function: FunctionDecl,
    module: &mut ObjectModule,
    ctx: &mut Context,
    fbx: &mut FunctionBuilderContext,
    ms_ctx: &mut MsContext,
) {
    let name = resolve_typename(&function.name);
    let mut linkage = Linkage::Preemptible;
    if function.is_extern {
        if matches!(function.block, Block::Empty) {
            linkage = Linkage::Import;
        } else {
            linkage = Linkage::Export;
        }
    }

    for (_k, v) in &function.arguments {
        let ty_name = resolve_typename(v);
        let ty = ms_ctx
            .type_registry
            .get(&ty_name)
            .expect(&format!("undefined type_name {}", ty_name));

        ctx.func.signature.params.push(ty.to_abi_param().unwrap());
    }
    {
        if !matches!(function.return_type, MsTokenType::Unknown) {
            let ty_name = resolve_typename(&function.return_type);
            let ty = ms_ctx
                .type_registry
                .get(&ty_name)
                .expect("invalid type_name");

            ctx.func.signature.returns.push(ty.to_abi_param().unwrap());
        }
    }

    let func_id = module
        .declare_function(&name, linkage, &ctx.func.signature)
        .unwrap();

    let declared_function = Rc::new(MsDeclaredFunction {
        func_id,
        arguments: function.arguments,
        rets: function.return_type,
        fn_type: if function.is_extern {
            FunctionType::Extern
        } else {
            FunctionType::Public
        },
    });
    if ms_ctx
        .current_module
        .fn_registry
        .registry
        .insert(name, declared_function.clone())
        .is_some()
    {
        panic!("Function declared twice in same module");
    }

    if matches!(function.block, Block::Empty) {
        ctx.clear();
        return;
    }
    let mut f = FunctionBuilder::new(&mut ctx.func, fbx);
    ms_ctx.var_scopes.new_scope();

    let entry_block = f.create_block();
    f.append_block_params_for_function_params(entry_block);
    f.switch_to_block(entry_block);

    let block_params = f.block_params(entry_block).to_vec();
    for ((arg_name, arg_type), value) in declared_function.arguments.iter().zip(block_params.iter())
    {
        let var = ms_ctx.new_variable();
        let ty_name = resolve_typename(&arg_type);
        let ty = ms_ctx
            .type_registry
            .get(&ty_name)
            .expect("invalid type_name")
            .clone();

        f.declare_var(var, ty.to_cl_type().unwrap());
        f.def_var(var, *value);
        ms_ctx.var_scopes.add_variable(
            arg_name.deref(),
            MsVar::new(ty.clone(), var, ty.to_string()),
        );
    }

    {
        compile_block(&function.block, module, &mut f, ms_ctx);
        let scope = ms_ctx.var_scopes.exit_scope().unwrap();
        drop_scope(scope, ms_ctx, &mut f, module);
    }
    f.seal_block(entry_block);
    f.finalize();
    module.define_function(func_id, ctx).unwrap();
    ctx.clear();
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
            drop_scope(vars, ms_ctx, fbx, module);
        }
        Block::Statements(stmts) => return compile_statements(&stmts, module, fbx, ms_ctx),
        Block::Continuos(blocks) => {
            for block in blocks {
                compile_block(block, module, fbx, ms_ctx);
            }
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

    if !variable.is_mutable() {
        panic!("{lhs} is marked immutable");
    }

    assert!(variable.ty == *rhs.ty());

    let _ = variable
        .ty
        .native()
        .expect("only native types can be assigned right now");
    let value = rhs.value(fbx);
    fbx.def_var(variable.c_var, value);
}

pub fn compile_cast(
    value: NodeResult,
    cast_to: MsType,

    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> NodeResult {
    match value.ty() {
        MsType::Native(nty) => {
            return NodeResult::Val(MsVal::new(
                nty.cast_to(value.value(fbx), &cast_to, fbx),
                cast_to.clone(),
                cast_to.to_string(),
            ))
        }
        _ => unimplemented!("only native types are castable"),
    }
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
                    Node::Term(Term::Type(term)) => resolve_typename(&term),
                    _ => unreachable!(),
                };
                let ty = ms_ctx
                    .type_registry
                    .registry
                    .get(ty_name.deref())
                    .expect(&format!("undefined type {}", ty_name));

                let value = compile_cast(value, ty.clone(), module, fbx, ms_ctx);

                return Some(value);
            } else if matches!(rule, Rule::assign) {
                // TODO: Check for enum unwrapping

                let variable_name = match lhs.deref() {
                    Node::Term(Term::Type(term)) => resolve_typename(&term),
                    _ => unreachable!("{:?}", lhs),
                };
                let rhs = compile_node(rhs, module, fbx, ms_ctx).unwrap();
                compile_assignment(&variable_name, rhs, module, fbx, ms_ctx);
                return None;
            } else {
                let lhs = compile_node(lhs, module, fbx, ms_ctx).unwrap();
                let rhs = compile_node(rhs, module, fbx, ms_ctx).unwrap();
                let op = rule_to_binary_operation(*rule)
                    .expect(&format!("unhandled rule to binary operation {:?}", rule));

                let MsType::Native(nty) = lhs.ty().clone() else {
                    panic!("only native types are supported for this binary operation");
                };
                let lval = lhs.value(fbx);
                let rval = rhs.value(fbx);
                let value = compile_binary_operation(op, lval, rval, nty, module, fbx, ms_ctx);

                let value =
                    NodeResult::Val(MsVal::new(value, MsType::Native(nty), nty.to_string()));

                return Some(value);
            }
        }
        Node::Unary(rule, node) => {
            if matches!(rule, Rule::deref) {
                let node = compile_node(node, module, fbx, ms_ctx).unwrap();

                let ptr_value = node.value(fbx);
                let val = fbx.ins().load(
                    node.ty().to_cl_type().unwrap(),
                    MemFlags::new(),
                    ptr_value,
                    0,
                );

                return Some(NodeResult::Val(MsVal::new(
                    val,
                    node.ty().clone(),
                    node.ty().to_string(),
                )));
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

            let fn_name = resolve_typename(fn_name);
            let func = ms_ctx
                .current_module
                .fn_registry
                .registry
                .get(&fn_name)
                .expect(&format!("undefined function {fn_name}"))
                .clone();
            let func_ref = module.declare_func_in_func(func.func_id, fbx.func);
            let mut call_arg_values = Vec::with_capacity(args.len());
            for arg in args {
                let val = compile_node(arg, module, fbx, ms_ctx).unwrap().value(fbx);
                call_arg_values.push(val);
            }
            let inst = fbx.ins().call(func_ref, &call_arg_values);
            let result = fbx.inst_results(inst);

            if !matches!(func.rets, MsTokenType::Unknown) && !result.is_empty() {
                let return_value = result[0];
                //     !void
                let ty_name = resolve_typename(&func.rets);
                let ty = ms_ctx
                    .current_module
                    .type_registry
                    .get(&ty_name)
                    .expect(&format!("undefined type {ty_name}"))
                    .clone();

                return Some(NodeResult::Val(MsVal::new(return_value, ty, ty_name)));
            }
        }
        Node::Term(term) => match term {
            Term::Array(vec) => todo!(),
            Term::Function(function_decl) => todo!(),
            Term::String(word_span) => {
                let mut s = word_span.to_string();
                let mut is_escaping = false;
                for c in word_span.chars() {
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
                let data_id = module
                    .declare_data(&data_name, Linkage::Preemptible, false, false)
                    .unwrap();
                let mut data_desc = DataDescription::new();
                data_desc.define(content.as_bytes().into());
                module.define_data(data_id, &data_desc).unwrap();

                let gl_value = module.declare_data_in_func(data_id, fbx.func);
                let ty = types::I64;
                let ptr = fbx.ins().global_value(ty, gl_value);
                let MsType::Struct(ty) = ms_ctx
                    .current_module
                    .type_registry
                    .get("StrSlice")
                    .unwrap()
                    .clone()
                else {
                    panic!("expected struct");
                };
                // str len: 8 + ptr: 8
                let stack_slot = fbx.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    ty.size() as u32,
                ));
                let size_of_data = fbx.ins().iconst(types::I64, content.len() as i64);

                let ty_i64 = MsType::Native(MsNativeType::I64);

                let struct_ptr = fbx.ins().stack_addr(types::I64, stack_slot, 0);
                ty.set_field(
                    &MsVal::new(struct_ptr, ty_i64.clone(), "i64"),
                    "len",
                    &MsVal::new(size_of_data, ty_i64.clone(), "i64"),
                    ms_ctx,
                    fbx,
                    module,
                );
                ty.set_field(
                    &MsVal::new(struct_ptr, ty_i64.clone(), "i64"),
                    "pointer",
                    &MsVal::new(ptr, ty_i64.clone(), "i64"),
                    ms_ctx,
                    fbx,
                    module,
                );

                let mut val = MsVal::new(
                    struct_ptr,
                    MsType::Ref(MsType::Struct(ty).into(), false),
                    "StrSlice",
                );

                return Some(NodeResult::Val(val));

                // fbx.ins().stack_store(size_of_data, stack_slot, 0);
                // fbx.ins().stack_store(ptr, stack_slot, 8);
                // let val = fbx.ins().stack_addr(types::I64, stack_slot, 0);

                // let ty = MsType::Ref(ty.into(), false);
                // return Some(NodeResult::Val(MsVal::new(val, ty)));
                todo!();
            }
            Term::Type(type_name) => {
                let var_name = resolve_typename(type_name);
                if let Some(var) = ms_ctx.var_scopes.find_variable(&var_name) {
                    return Some(NodeResult::Var(var.clone()));
                } else {
                    panic!("undefined {} word or type name in current scope", var_name);
                }
            }
            Term::I64(val) => {
                let ty = MsType::Native(MsNativeType::I64);
                let cty = ty.to_cl_type().unwrap();
                let val = fbx.ins().iconst(cty, *val);
                return Some(NodeResult::Val(MsVal::new(val, ty, "i64")));
            }
            Term::F64(val) => {
                let ty = MsType::Native(MsNativeType::F64);
                let cty = ty.to_cl_type().unwrap();
                let val = fbx.ins().f64const(*val);
                return Some(NodeResult::Val(MsVal::new(val, ty, "f64")));
            }
            Term::Char(c) => {
                let c = *c as i32; // utf8
                let ty = MsType::Native(MsNativeType::Char);
                let cty = ty.to_cl_type().unwrap();
                let val = fbx.ins().iconst(cty, c as i64);
                return Some(NodeResult::Val(MsVal::new(val, ty, "char")));
            }
            Term::Struct(ty, linear_map) => {
                let ty_name = resolve_typename(ty);
                let Some(MsType::Struct(struct_type)) = ms_ctx
                    .current_module
                    .type_registry
                    .registry
                    .get(ty_name.deref())
                    .cloned()
                else {
                    panic!("undefined struct {}", ty_name);
                };

                let stack_slot = fbx.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    struct_type.size() as u32,
                ));
                let ptr = fbx.ins().stack_addr(types::I64, stack_slot, 0);

                let ptr = MsVal::new(ptr, MsType::Struct(struct_type.clone()), ty_name);
                for (k, v) in linear_map {
                    let val = compile_node(v, module, fbx, ms_ctx).unwrap().to_ms_val(fbx);
                    struct_type.set_field(&ptr, k, &val, ms_ctx, fbx, module);
                }
            }
        },
        Node::None => {
            fbx.ins().nop();
        }
    }

    return None;
}

pub fn implicit_cast(
    var: NodeResult,
    ty: MsType,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> NodeResult {
    if *var.ty() == ty {
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
                let type_name = resolve_typename(expected_type);

                if !matches!(expected_type, MsTokenType::Unknown) {
                    let ty = ms_ctx
                        .current_module
                        .type_registry
                        .registry
                        .get(type_name.deref())
                        .unwrap()
                        .clone();
                    value = implicit_cast(value, ty, module, fbx, ms_ctx);
                }

                let node_value = value;
                let ty = node_value.ty().clone();
                let value = node_value.value(fbx);
                let variable = ms_ctx.new_variable();
                fbx.declare_var(variable, ty.to_cl_type().unwrap());
                fbx.def_var(variable, value);
                let mut variable = MsVar::new(ty.clone(), variable, node_value.type_name());
                variable.mark_mutability(*is_mutable);
                if let Some(old_variable) = ms_ctx.var_scopes.add_variable(var_name, variable) {
                    drop_variable(&old_variable, ms_ctx, fbx, module);
                }
            }
            Statement::Return(node) => {
                let ret_inst = if let Some(var) = compile_node(node, module, fbx, ms_ctx) {
                    let val = var.value(fbx);
                    fbx.ins().return_(&[val])
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
    ms_ctx
        .loop_scopes
        .new_loop(loop_name.map(|x| x.into()), fbx);

    compile_block(loop_block, module, fbx, ms_ctx);

    ms_ctx.loop_scopes.end_loop(loop_name, fbx);
    let scope = ms_ctx.var_scopes.exit_scope().unwrap();
    drop_scope(scope, &ms_ctx, fbx, module);
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
        drop_scope(scope, ms_ctx, fbx, module);
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
        drop_scope(scope, ms_ctx, fbx, module);
        if inst.is_none() {
            fbx.ins().jump(end_block, &[]);
        }
        // if compile_block(&block.block, module, fbx, ms_ctx).is_none() {
        //     fbx.ins().jump(end_block, &[]);
        // }
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
        drop_scope(scope, ms_ctx, fbx, module);
        if inst.is_none() {
            fbx.ins().jump(self.end_block, &[]);
        }

        // if compile_block(&block, module, fbx, ms_ctx).is_none() {
        //     fbx.ins().jump(self.end_block, &[]);
        // }
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
    let val = rng.gen_range(0..=3);
    match val {
        0 => rng.gen_range('a'..='z'),
        1 => rng.gen_range('A'..='Z'),
        2 => rng.gen_range('0'..='9'),
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
    for i in 1..len {
        let c = random_readable_char(&mut rng);
        w.write_char(c).unwrap();
    }
}
