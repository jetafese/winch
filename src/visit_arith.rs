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
    let v = nondet_u8();
    match v {
        0 => visit_i32_const(),
        1 => visit_i64_const(),
        2 => visit_i32_add(),
        3 => visit_i64_add(),
        4 => visit_i32_sub(),
        5 => visit_i64_sub(),
        6 => visit_i32_mul(),
        7 => visit_i64_mul(),
        _ => visit_i32_div_s(),
    }
    return 0;
}

#[no_mangle]
fn visit_i32_const() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let mut codegen_context = proof_core::setup_context(&vmoffsets);
    // SUT
    let val = nondet_i32();
    codegen_context.stack.push(Val::i32(val));
    codegen_context.stack.peek().expect("value at stack top");
    codegen_context.stack.pop();
    assert(codegen_context.stack.inner().is_empty());
}

#[no_mangle]
fn visit_i64_const() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let mut codegen_context = proof_core::setup_context(&vmoffsets);
    // SUT
    let val = nondet_i64();
    codegen_context.stack.push(Val::i64(val));
}

#[no_mangle]
fn visit_i32_add() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    // invariant: top value on stack can be const/reg, second value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i32(dst)));
    let val = nondet_i32();
    emission_context.stack.push(Val::I32(val));
    let res = emission_context.i32_binop(&mut masm, |masm, dst, src, size| {
        masm.add(writable!(dst), dst, src, size)?;
        Ok(TypedReg::i32(dst))
    });
    assert(res.is_ok());
}

#[no_mangle]
fn visit_i64_add() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    // invariant: top value on stack can be const/reg, second value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i64(dst)));
    let val = nondet_i64();
    emission_context.stack.push(Val::I64(val));
    let res = emission_context.i64_binop(&mut masm, |masm, dst, src, size| {
        masm.add(writable!(dst), dst, src, size)?;
        Ok(TypedReg::i64(dst))
    });
    assert(res.is_ok());
}

#[no_mangle]
fn visit_i32_sub() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    // invariant: top value on stack can be const/reg, second value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i64(dst)));
    let val = nondet_i64();
    emission_context.stack.push(Val::I64(val));
    let res = emission_context.i32_binop(&mut masm, |masm, dst, src, size| {
        masm.sub(writable!(dst), dst, src, size)?;
        Ok(TypedReg::i32(dst))
    });
    assert(res.is_ok());
}

#[no_mangle]
fn visit_i64_sub() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    // invariant: top value on stack can be const/reg, second value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i64(dst)));
    let val = nondet_i64();
    emission_context.stack.push(Val::I64(val));
    let res = emission_context.i64_binop(&mut masm, |masm, dst, src, size| {
        masm.sub(writable!(dst), dst, src, size)?;
        Ok(TypedReg::i64(dst))
    });
    assert(res.is_ok());
}

#[no_mangle]
fn visit_i32_mul() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    // invariant: top value on stack can be const/reg, second value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i64(dst)));
    let val = nondet_i64();
    emission_context.stack.push(Val::I64(val));
    let res = emission_context.i32_binop(&mut masm, |masm, dst, src, size| {
        masm.mul(writable!(dst), dst, src, size)?;
        Ok(TypedReg::i32(dst))
    });
    assert(res.is_ok());
}

#[no_mangle]
fn visit_i64_mul() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    // invariant: top value on stack can be const/reg, second value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i64(dst)));
    let val = nondet_i64();
    emission_context.stack.push(Val::I64(val));
    let res = emission_context.i64_binop(&mut masm, |masm, dst, src, size| {
        masm.mul(writable!(dst), dst, src, size)?;
        Ok(TypedReg::i64(dst))
    });
    assert(res.is_ok());
}

#[no_mangle]
fn visit_i32_div_s() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    // invariant: top value on stack can be const/reg, second value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i64(dst)));
    let val = nondet_i64();
    emission_context.stack.push(Val::I64(val));
    // SEA_TODO: The function div is not sensitive to the operand size
    let res = masm.div(&mut emission_context, DivKind::Signed, OperandSize::S32);
    assert(res.is_ok());
    emission_context.stack.peek().expect("value at stack top");
    emission_context.stack.pop();
    assert(emission_context.stack.inner().is_empty());
}