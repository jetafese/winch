//! Unit proof collection for Winch.

#![no_builtins]
#![no_std]
#![no_main]
#![feature(default_alloc_error_handler)]
#![feature(lang_items)]

use codegen::{Callee, CodeGenContext, FnCall, FuncEnv, Prologue};
use libc_alloc::LibcAlloc;

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

#[link(name="c")]
extern "C" {}

#[cfg(not(test))]
#[lang = "eh_personality"]
extern "C" fn rust_eh_personality() {}

// /// Workaround for rustc bug: https://github.com/rust-lang/rust/issues/47493
// ///
// /// It shouldn't even be possible to reach this function, thanks to panic=abort,
// /// but libcore is compiled with unwinding enabled and that ends up making unreachable
// /// references to this.
#[no_mangle]
extern "C" fn _Unwind_Resume() -> ! {
    unreachable!("Unwinding not supported");
}

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
use masm::{DivKind, IntCmpKind, MacroAssembler, OperandSize, RegImm};
use isa::{reg::{self, writable, Reg}, x64::abi::X64ABI};
use crate::abi::ABI;
use regalloc2::PReg;
use regset::RegBitSet;
use seahorn_stubs::{assert, assume, error, nondet_i32, nondet_i64, nondet_u32, nondet_u8};
use smallvec::SmallVec;
use self::isa::x64::regs::{ALL_FPR, ALL_GPR, MAX_FPR, MAX_GPR, NON_ALLOCATABLE_FPR, NON_ALLOCATABLE_GPR};
use stack::{Stack, TypedReg, Val};
use frame::{DefinedLocals, Frame};
use wasmtime_environ::{FuncIndex, VMOffsets, WasmFuncType, WasmRefType, WasmValType::{self, *}};

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
        _ => masm_new(),
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
        4 => visit_i32_sub(),
        5 => visit_i64_sub(),
        6 => visit_i32_mul(),
        7 => visit_i64_mul(),
        8 => visit_i32_div_s(),
        9 => visit_cmp_ops(),
        10 => visit_call(),
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

    // TODO: needs a creative way to verify different signatures
    let sig = WasmFuncType::new(
        [I32, I64, I32, I64, I32, I32, I64, I32].into(),
        [I32, I32, I32, I64, I32, I32, I64, I32].into(),
    );
    let abi_sig = X64ABI::sig(&sig, &isa::CallingConvention::Default);
    // TODO: should be able to pass values allocate nd locals
    let defined_locals = DefinedLocals::new::<X64ABI>();
    let frame = Frame::new::<X64ABI>(&abi_sig, &defined_locals.unwrap()).unwrap();

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
fn setup_masm() -> isa::x64::masm::MacroAssembler {
    let isa_flags = cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = cranelift_codegen::settings::Flags::new();
    let ptr_size = 8;
    let masm_64 = isa::x64::masm::MacroAssembler::new(ptr_size, shared_flags, isa_flags);
    return masm_64.unwrap();
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
    let mut masm = setup_masm();
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
    let mut masm = setup_masm();
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
    let codegen_context = setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = setup_masm();
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
    let codegen_context = setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = setup_masm();
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
    let codegen_context = setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = setup_masm();
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
    let codegen_context = setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = setup_masm();
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
    let codegen_context = setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = setup_masm();
    // SUT
    // invariant: top value on stack can be const/reg, second value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i64(dst)));
    let val = nondet_i64();
    emission_context.stack.push(Val::I64(val));
    // TODO: The function div is not sensitive to the operand size
    let res = masm.div(&mut emission_context, DivKind::Signed, OperandSize::S32);
    assert(res.is_ok());
    emission_context.stack.peek().expect("value at stack top");
    emission_context.stack.pop();
    assert(emission_context.stack.inner().is_empty());
}

#[no_mangle]
fn visit_cmp_ops() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = setup_masm();
    // SUT
    // invariant: top value on stack can be const/reg, second value should be dst reg
    let dst = Reg(PReg::new(2, regalloc2::RegClass::Int));
    emission_context.stack.push(Val::Reg(TypedReg::i64(dst)));
    let val = nondet_i64();
    emission_context.stack.push(Val::I64(val));
    // select operation
    let v = nondet_u8();
    let kind = match v {
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
    };
    // call functions
    let v2 = nondet_u8();
    let res = match v2 {
        0 => {
            emission_context.i32_binop(&mut masm, |masm, dst, src, size| {
                masm.cmp_with_set(writable!(dst), src, kind, size)?;
                Ok(TypedReg::i32(dst))
            })
        },
        _ => {
            emission_context.i64_binop(&mut masm, move |masm, dst, src, size| {
                masm.cmp_with_set(writable!(dst), src, kind, size)?;
                Ok(TypedReg::i32(dst)) // Return value for comparisons is an `i32`.
            })
        },
    };
    assert(res.is_ok());
    emission_context.stack.peek().expect("value at stack top");
    emission_context.stack.pop();
    assert(emission_context.stack.inner().is_empty());
}

#[no_mangle]
fn visit_call() {
    // setup context
    let vmoffsets = VMOffsets::new();
    let codegen_context = setup_context(&vmoffsets);
    let mut emission_context = codegen_context.for_emission();
    let mut masm = setup_masm();
    // SUT
    let index = nondet_u32();
    let mut env = FuncEnv::new(vmoffsets, WasmValType::Ref(WasmRefType::FUNCREF));
    let import = nondet_u32() % 2 == 0;
    let func_index = FuncIndex::from_u32(index);
    let callee = if import { Callee::Import(func_index) } else { Callee::Local(func_index) };
    FnCall::emit(&mut env, &mut masm, &mut emission_context, callee);
}