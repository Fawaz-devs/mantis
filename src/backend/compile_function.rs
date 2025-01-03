use std::fmt::Write;
use std::ops::Deref;
use std::rc::Rc;

use crate::frontend::tokens::MsContext;
use crate::registries::functions::{FunctionType, MsDeclaredFunction};
use crate::registries::types::{MsNativeType, MsType};
use crate::registries::variable::{MsVal, MsVar};
use crate::registries::MsRegistryExt;
use crate::scope::{drop_scope, drop_variable};
use cranelift::{codegen::Context, prelude::*};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::ObjectModule;
use mantis_expression::pratt::{Block, FunctionDecl, Node, Rule, Term, WordSpan};
use mantis_expression::pratt::{IfElseChain, Statement, Type as MsTokenType};

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
        MsTokenType::Unknown => todo!(),
        MsTokenType::Word(word_span) => word_span.as_str().into(),
        MsTokenType::Ref(_, _) => todo!(),
    }
}

pub fn type_name_into_cranelift_type(ty_name: &str, ctx: &MsContext) -> Option<types::Type> {
    todo!()
}

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
        let ty = type_name_into_cranelift_type(&resolve_typename(v), &ms_ctx)
            .expect("invalid type_name");

        ctx.func.signature.params.push(AbiParam::new(ty));
    }
    {
        if let Some(ty) =
            type_name_into_cranelift_type(&resolve_typename(&function.return_type), &ms_ctx)
        {
            ctx.func.signature.returns.push(AbiParam::new(ty));
        }
    }

    let func_id = module
        .declare_function(&name, linkage, &ctx.func.signature)
        .unwrap();
    if ms_ctx
        .current_module
        .fn_registry
        .registry
        .insert(
            name,
            Rc::new(MsDeclaredFunction {
                func_id,
                arguments: function.arguments,
                rets: function.return_type,
                fn_type: if function.is_extern {
                    FunctionType::Extern
                } else {
                    FunctionType::Public
                },
            }),
        )
        .is_some()
    {
        panic!("Function declared twice in same module");
    }

    if matches!(function.block, Block::Empty) {
        ctx.clear();
        return;
    }
    let mut f = FunctionBuilder::new(&mut ctx.func, fbx);
    compile_block(&function.block, module, &mut f, ms_ctx);
    f.seal_all_blocks();
    f.finalize();
    module.define_function(func_id, ctx).unwrap();
    ctx.clear();
}

pub fn compile_block(
    block: &Block,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) {
    match block {
        Block::Closed(block) => {
            ms_ctx.var_scopes.new_scope();
            compile_block(&block, module, fbx, ms_ctx);
            let vars = ms_ctx.var_scopes.exit_scope().unwrap();
            drop_scope(vars, ms_ctx, fbx, module);
        }
        Block::Statements(stmts) => compile_statements(&stmts, module, fbx, ms_ctx),
        Block::Continuos(blocks) => {
            for block in blocks {
                compile_block(block, module, fbx, ms_ctx);
            }
        }
        Block::IfElseChain(if_else_chain) => {
            compile_if_else_chain(if_else_chain, module, fbx, ms_ctx);
        }
        Block::Loop(word_span, block) => {
            compile_loop(word_span.clone(), block, module, fbx, ms_ctx);
        }
        Block::Match(node, vec) => todo!("match cases unimplemented"),
        Block::Empty => {
            log::warn!("Empty block, doing nothing")
        }
    }
}

pub fn compile_binary_operation(
    rule: Rule,
    lhs: MsVar,
    rhs: MsVar,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> MsVar {
    todo!()
}

pub fn compile_node(
    node: &Node,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> Option<MsVar> {
    match node {
        Node::Binary(rule, lhs, rhs) => {
            let lhs = compile_node(lhs, module, fbx, ms_ctx).unwrap();
            let rhs = compile_node(rhs, module, fbx, ms_ctx).unwrap();
            let value = compile_binary_operation(*rule, lhs, rhs, module, fbx, ms_ctx);
            return Some(value);
        }
        Node::Unary(rule, node) => {
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
                let var = compile_node(arg, module, fbx, ms_ctx).unwrap().c_var;
                let val = fbx.use_var(var);
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
                let var = ms_ctx.new_variable();
                fbx.declare_var(var, ty.to_cl_type().unwrap());
                fbx.def_var(var, return_value);

                return Some(MsVar::new(ty, var));
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
                let ty = ms_ctx
                    .current_module
                    .type_registry
                    .registry
                    .get("str")
                    .unwrap()
                    .clone();
                // str len: 8 + ptr: 8
                let stack_slot = fbx.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    ty.size() as u32,
                ));

                let size_of_data = fbx.ins().iconst(types::I64, content.len() as i64);
                fbx.ins().stack_store(size_of_data, stack_slot, 0);
                fbx.ins().stack_store(ptr, stack_slot, 8);
                let val = fbx.ins().stack_addr(types::I64, stack_slot, 0);
                let ty = MsType::Ref(ty.into(), false);
                let var = ms_ctx.new_variable();
                fbx.declare_var(var, types::I64);
                fbx.def_var(var, val);
                return Some(MsVar::new(ty, var));
            }
            Term::Type(type_name) => {
                let var_name = resolve_typename(type_name);
                if let Some(var) = ms_ctx.var_scopes.find_variable(&var_name) {
                    return Some(var.clone());
                } else {
                    panic!("undefined {} word or type name in current scope", var_name);
                }
            }
            Term::I64(val) => {
                let ty = MsType::Native(MsNativeType::I64);
                let cty = ty.to_cl_type().unwrap();
                let val = fbx.ins().iconst(cty, *val);
                let var = ms_ctx.new_variable();
                fbx.declare_var(var, cty);
                fbx.def_var(var, val);
                return Some(MsVar::new(ty, var));
            }
            Term::F64(val) => {
                let ty = MsType::Native(MsNativeType::F64);
                let cty = ty.to_cl_type().unwrap();
                let val = fbx.ins().f64const(*val);
                let var = ms_ctx.new_variable();
                fbx.declare_var(var, cty);
                fbx.def_var(var, val);
                return Some(MsVar::new(ty, var));
            }
            Term::Char(c) => {
                let c = *c as i32; // utf8

                let node = Node::Binary(
                    Rule::cast,
                    Box::new(Node::Term(Term::I64(c as i64))),
                    Box::new(Node::Term(Term::Type(MsTokenType::Word("i32".into())))),
                );

                let mut var = compile_node(&node, module, fbx, ms_ctx).unwrap();
                var.ty = MsType::Native(MsNativeType::Char);
                return Some(var);
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

                let ptr = MsVal::new(ptr, MsType::Struct(struct_type.clone()));
                for (k, v) in linear_map {
                    let var = compile_node(v, module, fbx, ms_ctx).unwrap();
                    let val = fbx.use_var(var.c_var);
                    let val = MsVal::new(val, var.ty.clone());
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
    var: MsVar,
    ty: MsType,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) -> MsVar {
    if var.ty == ty {
        return var;
    }

    todo!()
}

pub fn compile_statements(
    statements: &[Statement],
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) {
    for stmt in statements {
        match stmt {
            Statement::Let(word_span, node, expected_type, is_mutable) => {
                let var_name = word_span.as_str();
                let mut variable = compile_node(node, module, fbx, ms_ctx).unwrap();
                let type_name = resolve_typename(expected_type);

                if !matches!(expected_type, MsTokenType::Unknown) {
                    let ty = ms_ctx
                        .current_module
                        .type_registry
                        .registry
                        .get(type_name.deref())
                        .unwrap()
                        .clone();
                    variable = implicit_cast(variable, ty, module, fbx, ms_ctx);
                }

                if let Some(old_variabe) = ms_ctx.var_scopes.add_variable(var_name, variable) {
                    drop_variable(old_variabe, ms_ctx, fbx, module);
                }
            }
            Statement::Return(node) => {
                if let Some(var) = compile_node(node, module, fbx, ms_ctx) {
                    let val = fbx.use_var(var.c_var);
                    fbx.ins().return_(&[val]);
                } else {
                    fbx.ins().return_(&[]);
                }
            }
            Statement::Break(word_span) => todo!(),
            Statement::Continue(word_span) => todo!(),
            Statement::Expr(node) => {
                compile_node(node, module, fbx, ms_ctx);
            }
        }
    }
}

pub fn compile_loop(
    loop_name: Option<WordSpan>,
    loop_block: &Block,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) {
    todo!();
}

pub fn compile_if_else_chain(
    if_else_chain: &IfElseChain,
    module: &mut ObjectModule,
    fbx: &mut FunctionBuilder,
    ms_ctx: &mut MsContext,
) {
    todo!()
}

pub fn random_readable_char(mut rng: impl rand::Rng) -> char {
    let val = rng.gen::<usize>() % 4;
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
    for i in 0..len {
        let c = random_readable_char(&mut rng);
        w.write_char(c);
    }
}
