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
    general();
    return 0;
}

#[no_mangle]
fn general() {
    let v = nondet_u8();
    match v {
        0 => masm_new(),
        _ => checked_uadd(),
    }
}

#[no_mangle]
fn masm_new() {
    let isa_flags = cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = cranelift_codegen::settings::Flags::new();
    let ptr_size = nondet_u8();
    // invariant: ptr_size has to be equal to 8 so that the next line doesn't panic
    assume(ptr_size == 8);
    let masm_64 = isa::x64::masm::MacroAssembler::new(ptr_size, shared_flags, isa_flags);
}

#[no_mangle]
fn checked_uadd() {
    let isa_flags = cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = cranelift_codegen::settings::Flags::new();
    let ptr_size = 8;
    let masm_64 = isa::x64::masm::MacroAssembler::new(ptr_size, shared_flags, isa_flags);
    let mut masm = masm_64.unwrap();
    // invariant: dst != src
    let src = Reg(PReg::new(2, regalloc2::RegClass::Int));
    let dst_val = nondet_i32();
    let dst = RegImm::Imm(masm::Imm::I64(dst_val as u64));
    let res = masm.checked_uadd(Writable::from_reg(src), src, dst, masm::OperandSize::S32, TrapCode::INTEGER_OVERFLOW);
    assert(res.is_ok());
}