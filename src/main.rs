//! Unit proof collection for Winch.

#![no_builtins]
#![no_std]
#![no_main]
#![feature(default_alloc_error_handler)]

use libc_alloc::LibcAlloc;

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

#[link(name="c")]
extern "C" {}

pub mod isa;
// pub use isa::*;
mod masm;
mod regalloc;
mod regset;
mod codegen;
mod stack;
mod frame;
mod abi;
// mod visitor;
/* stubbed libraries */
mod cranelift_codegen;
mod wasmtime_environ;
mod regalloc2;
mod target_lexicon;

use cranelift_codegen::{ir::TrapCode, Writable};
use masm::{MacroAssembler, RegImm};
use isa::reg::{self, Reg};
use regalloc2::PReg;
use regset::RegBitSet;
use seahorn_stubs::{assert, assume, error, nondet_i32, nondet_i64, nondet_u32, nondet_u8};
use self::isa::x64::regs::{ALL_FPR, ALL_GPR, MAX_FPR, MAX_GPR, NON_ALLOCATABLE_FPR, NON_ALLOCATABLE_GPR};
use stack::{Stack, Val};
use frame::Frame;
use wasmtime_environ::VMOffsets;

use core::panic::PanicInfo;

#[panic_handler]
#[no_mangle]
fn panic(_panic: &PanicInfo<'_>) -> ! {
    error();
}

#[allow(unused)]
#[no_mangle]
pub extern fn main() {
    let v = nondet_u8();
    match v {
        0 => general(),
        _ => visitors(),
    }
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
fn visitors() {
    let v = nondet_u8();
    match v {
        0 => visit_i32_const(),
        1 => visit_i64_const(),
        _ => (),
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
    let src = Reg(PReg::new(2, regalloc2::RegClass::Int));
    let dst_val = nondet_i32();
    let dst = RegImm::Imm(masm::Imm::I64(dst_val as u64));
    masm_64.unwrap().checked_uadd(Writable::from_reg(src), src, dst, masm::OperandSize::S32, TrapCode::INTEGER_OVERFLOW);
}

#[no_mangle]
fn visit_i32_const() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let stack = Stack::new();
    let frame = Frame::new().unwrap();

    let gpr = RegBitSet::int(
        ALL_GPR.into(),
        NON_ALLOCATABLE_GPR.into(),
        usize::try_from(MAX_GPR).unwrap(),
    );
    let fpr = RegBitSet::float(
        ALL_FPR.into(),
        NON_ALLOCATABLE_FPR.into(),
        usize::try_from(MAX_FPR).unwrap(),
    );
    let regalloc = regalloc::RegAlloc::from(gpr, fpr);
    let mut codegen_context = codegen::CodeGenContext::new(regalloc, stack, frame, &vmoffsets);
    let val = nondet_i32();
    // SUT
    codegen_context.stack.push(Val::i32(val));
}

#[no_mangle]
fn visit_i64_const() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let stack = Stack::new();
    let frame = Frame::new().unwrap();

    let gpr = RegBitSet::int(
        ALL_GPR.into(),
        NON_ALLOCATABLE_GPR.into(),
        usize::try_from(MAX_GPR).unwrap(),
    );
    let fpr = RegBitSet::float(
        ALL_FPR.into(),
        NON_ALLOCATABLE_FPR.into(),
        usize::try_from(MAX_FPR).unwrap(),
    );
    let regalloc = regalloc::RegAlloc::from(gpr, fpr);
    let mut codegen_context = codegen::CodeGenContext::new(regalloc, stack, frame, &vmoffsets);
    let val = nondet_i64();
    // SUT
    codegen_context.stack.push(Val::i64(val));
}