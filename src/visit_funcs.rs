//! Unit proof collection for Winch.

#![no_builtins]
#![no_std]
#![no_main]
#![feature(default_alloc_error_handler)]
#![feature(lang_items)]

use abi::wasm_sig;
use codegen::{Callee, CodeGenContext, FnCall, FuncEnv, Prologue};
use libc_alloc::LibcAlloc;

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
use wasmtime_environ::{FuncIndex, VMOffsets, WasmFuncType, WasmRefType, WasmValType::{self, *}};

#[allow(unused)]
#[no_mangle]
pub extern fn main() -> i32 {
    let v = nondet_u8();
    match v {
       0 => visit_call_0_0(),
       1 => visit_call_8_0(),
       _ => int_abi_sig_8_0(),
    }
    return 0;
}

// We have arranged for func index 0 to have 0 args and 0 returns,
// and func index 1 to have 8 args and 0 returns.

#[no_mangle]
fn visit_call_0_0() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    let index = 0; // the 0-0 function
    let mut env = FuncEnv::new(vmoffsets, WasmValType::Ref(WasmRefType::FUNCREF));
    let func_index = FuncIndex::from_u32(index);
    let callee = Callee::Local(func_index);
    FnCall::emit(&mut env, &mut masm, &mut emission_context, callee, 0, 0);
    // SEA_TODO: Need to run with bound=2 to ensure we hit next line when in debug mode
    // lines like zip of two iterators are problematic: src/codegen/call.rs:assign_context_args
    assert(false); 
}

#[no_mangle]
fn visit_call_8_0() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = proof_core::setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = proof_core::setup_masm();
    // SUT
    let index = 1; // the 8-0 function
    let mut env = FuncEnv::new(vmoffsets, WasmValType::Ref(WasmRefType::FUNCREF));
    let func_index = FuncIndex::from_u32(index);
    let callee = Callee::Local(func_index);
    FnCall::emit(&mut env, &mut masm, &mut emission_context, callee, 8, 0);
    // SEA_TODO: Need to run with bound=2 to ensure we hit next line when in debug mode
    // lines like zip of two iterators are problematic: src/codegen/call.rs:assign_context_args
    assert(false)
}

#[no_mangle]
fn int_abi_sig_8_0() {
    let mut prms = NoResizableVec::<WasmValType>::new(8);
    prms.push(I32);
    prms.push(I64);
    prms.push(I32);
    prms.push(I64);
    prms.push(I32);
    prms.push(I32);
    prms.push(I64);
    prms.push(I32);
    let rts = NoResizableVec::<WasmValType>::new(0);
    let wasm_sig = WasmFuncType::new(prms, rts);
    let sig = X64ABI::sig(&wasm_sig, &isa::CallingConvention::Default, 8, 0);
    let params = sig.params;
    match_reg_arg(params.get(0).unwrap(), I32, regs::rdi());
    match_reg_arg(params.get(1).unwrap(), I64, regs::rsi());
    match_reg_arg(params.get(2).unwrap(), I32, regs::rdx());
    match_reg_arg(params.get(3).unwrap(), I64, regs::rcx());
    match_reg_arg(params.get(4).unwrap(), I32, regs::r8());
    match_reg_arg(params.get(5).unwrap(), I32, regs::r9());
    match_stack_arg(params.get(6).unwrap(), I64, 0);
    match_stack_arg(params.get(7).unwrap(), I32, 8);
    //// should fail
    // match_stack_arg(params.get(7).unwrap(), I64, 8);
    // assert(false);
}

#[track_caller]
#[no_mangle]
fn match_reg_arg(abi_arg: &abi::ABIOperand, expected_ty: WasmValType, expected_reg: Reg) {
    match abi_arg {
        &abi::ABIOperand::Reg { reg, ty, .. } => {
            assert(reg == expected_reg);
            assert(ty == expected_ty);
        }
        stack => panic!("Expected reg argument, got {stack:?}"),
    }
}

#[track_caller]
#[no_mangle]
fn match_stack_arg(abi_arg: &abi::ABIOperand, expected_ty: WasmValType, expected_offset: u32) {
    match abi_arg {
        &abi::ABIOperand::Stack { offset, ty, .. } => {
            assert(offset == expected_offset);
            assert(ty == expected_ty);
        }
        reg => panic!("Expected stack argument, got {reg:?}"),
    }
}