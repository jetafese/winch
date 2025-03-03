//! Unit proof collection for Winch.

#![no_builtins]
#![no_std]
#![no_main]
#![feature(default_alloc_error_handler)]
#![feature(lang_items)]

use codegen::{CodeGenContext, Prologue};
use libc_alloc::LibcAlloc;

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

#[link(name="c")]
extern "C" {}

#[cfg(not(test))]
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}

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
use isa::reg::{self, writable, Reg};
use regalloc2::PReg;
use regset::RegBitSet;
use seahorn_stubs::{assert, assume, error, nondet_i32, nondet_i64, nondet_u32, nondet_u8};
use self::isa::x64::regs::{ALL_FPR, ALL_GPR, MAX_FPR, MAX_GPR, NON_ALLOCATABLE_FPR, NON_ALLOCATABLE_GPR};
use stack::{Stack, TypedReg, Val};
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
        2 => visit_i32_add(),
        3 => visit_i64_add(),
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
    let mut masm = masm_64.unwrap();
    // invariant: dst != src
    let src = Reg(PReg::new(2, regalloc2::RegClass::Int));
    let dst_val = nondet_i32();
    let dst = RegImm::Imm(masm::Imm::I64(dst_val as u64));
    let res = masm.checked_uadd(Writable::from_reg(src), src, dst, masm::OperandSize::S32, TrapCode::INTEGER_OVERFLOW);
    assert(res.is_ok());
}

#[no_mangle]
fn setup_context<'a>(vmoffsets: &'a VMOffsets) -> CodeGenContext<'a, Prologue> {
    // let vmoffsets = VMOffsets::new();
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
    return codegen_context;
}

#[no_mangle]
fn visit_i32_const() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let mut codegen_context = setup_context(&vmoffsets);
    // SUT
    let val = nondet_i32();
    codegen_context.stack.push(Val::i32(val));
}

#[no_mangle]
fn visit_i64_const() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let mut codegen_context = setup_context(&vmoffsets);
    // SUT
    let val = nondet_i64();
    codegen_context.stack.push(Val::i64(val));
}

#[no_mangle]
fn visit_i32_add() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let isa_flags = cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = cranelift_codegen::settings::Flags::new();
    let ptr_size = 8;
    let masm_64 = isa::x64::masm::MacroAssembler::new(ptr_size, shared_flags, isa_flags);
    let mut masm = masm_64.unwrap();
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
    let codegen_context = setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let isa_flags = cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = cranelift_codegen::settings::Flags::new();
    let ptr_size = 8;
    let masm_64 = isa::x64::masm::MacroAssembler::new(ptr_size, shared_flags, isa_flags);
    let mut masm = masm_64.unwrap();
    // SUT
    // invariant: top value on stack can be const/reg
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
