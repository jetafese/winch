//! Unit proof collection for Winch.

#![no_builtins]
#![no_std]
#![no_main]
#![feature(default_alloc_error_handler)]
#![feature(lang_items)]

pub mod isa;
mod masm;
mod regalloc;
mod regset;
mod codegen;
mod stack;
mod frame;
mod abi;
/* stubbed libraries */
mod cranelift_codegen;
mod wasmtime_environ;
mod regalloc2;
mod target_lexicon;
mod no_resizeable_vec;
mod proof_core;

use cranelift_codegen::{ir::TrapCode, Writable};
use masm::{DivKind, IntCmpKind, MacroAssembler, OperandSize, RegImm};
use isa::{reg::{self, writable, Reg}, x64::{abi::X64ABI, regs}};
use crate::abi::ABI;
use regalloc2::PReg;
use regset::RegBitSet;
use seahorn_stubs::{assert, assume, error, nondet_i32, nondet_i64, nondet_u32, nondet_u8};
use crate::no_resizeable_vec::NoResizableVec;
use self::isa::x64::regs::{ALL_FPR, ALL_GPR, MAX_FPR, MAX_GPR, NON_ALLOCATABLE_FPR, NON_ALLOCATABLE_GPR};
use stack::{Stack, TypedReg, Val};
use frame::{DefinedLocals, Frame};
use codegen::{Callee, CodeGenContext, FnCall, FuncEnv, Prologue};
use wasmtime_environ::{FuncIndex, VMOffsets, WasmFuncType, WasmRefType, WasmValType::{self, *}};

#[allow(unused)]
#[no_mangle]
pub extern fn main() -> i32 {
    visit_cmp_ops();
    return 0;
}

#[no_mangle]
fn visit_cmp_ops() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    // invariant: second stack value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i32(dst)));
    // select operation
    let kind = symbol_kind();
    // call functions by operation width
    let v2 = nondet_u8();
    let res = match v2 {
        0 => {
            // invariant: top value on stack can be const/reg
            let val = nondet_i32();
            emission_context.stack.push(Val::I32(val));
            emission_context.i32_binop(&mut masm, |masm, dst, src, size| {
                masm.cmp_with_set(writable!(dst), src, kind, size)?;
                Ok(TypedReg::i32(dst))
            })
        },
        _ => {
            // invariant: top value on stack can be const/reg
            let val = nondet_i64();
            emission_context.stack.push(Val::I64(val));
            emission_context.i64_binop(&mut masm, |masm, dst, src, size| {
                masm.cmp_with_set(writable!(dst), src, kind, size)?;
                Ok(TypedReg::i32(dst)) // Return value for cmp is an `i32`.
            })
        },
    };
    assert(res.is_ok());
    emission_context.stack.peek().expect("value at stack top");
    emission_context.stack.pop().unwrap().is_i32_const();
    assert(emission_context.stack.inner().is_empty());
}

fn symbol_kind() -> IntCmpKind {
    let v = nondet_u8();
    match v {
        0 => IntCmpKind::Eq,
        1 => IntCmpKind::Ne,
        2 => IntCmpKind::GeS,
        3 => IntCmpKind::GeU,
        4 => IntCmpKind::GtS,
        5 => IntCmpKind::GtU,
        6 => IntCmpKind::LeS,
        7 => IntCmpKind::LeU,
        8 => IntCmpKind::LtS,
        _ => IntCmpKind::LtU,
    }
}
