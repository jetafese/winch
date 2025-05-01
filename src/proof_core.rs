//! Unit proof collection for Winch.

#![no_builtins]
#![no_std]
#![no_main]
#![feature(default_alloc_error_handler)]
#![feature(lang_items)]

use crate::abi::wasm_sig;
use crate::codegen::{Callee, CodeGenContext, FnCall, FuncEnv, Prologue};
use libc_alloc::LibcAlloc;

#[global_allocator]
pub static ALLOCATOR: LibcAlloc = LibcAlloc;

#[link(name="c")]
extern "C" {}

use crate::cranelift_codegen::{ir::TrapCode, Writable};
use crate::masm::{DivKind, IntCmpKind, MacroAssembler, OperandSize, RegImm};
use crate::isa::{
    reg::{self, writable, Reg}, 
    x64::{abi::X64ABI, 
        regs::{ALL_FPR, ALL_GPR, MAX_FPR, MAX_GPR, NON_ALLOCATABLE_FPR, NON_ALLOCATABLE_GPR}
    }
};
use crate::abi::ABI;
use crate::regalloc2::PReg;
use crate::regset::RegBitSet;
use seahorn_stubs::{assert, assume, error, nondet_i32, nondet_i64, nondet_u32, nondet_u8};
use crate::no_resizeable_vec::NoResizableVec;
use crate::stack::{Stack, TypedReg, Val};
use crate::frame::{DefinedLocals, Frame};
use crate::wasmtime_environ::{
    FuncIndex, VMOffsets, WasmFuncType, WasmRefType,
    WasmValType::{self, *}
};
use core::panic::PanicInfo;

#[panic_handler]
#[no_mangle]
pub fn panic(_panic: &PanicInfo<'_>) -> ! {
    error();
}

#[no_mangle]
pub fn setup_context<'a>(vmoffsets: &'a VMOffsets) -> CodeGenContext<'a, Prologue> {
    let stack = Stack::new();
    // SEA_TODO: src/codegen/env.rs:callee_sig is where the action is
    let sig = WasmFuncType::new(
        NoResizableVec::<WasmValType>::new(0),
        NoResizableVec::<WasmValType>::new(0)
    );
    // let abi_sig = X64ABI::sig(&sig, &isa::CallingConvention::Default);
    let abi_sig = wasm_sig::<X64ABI>(&sig, 0, 0);
    // SEA_TODO: should be able to pass values allocate nd locals
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
    let regalloc = crate::regalloc::RegAlloc::from(gpr, fpr);
    let mut codegen_context = crate::codegen::CodeGenContext::new(regalloc, stack, frame, &vmoffsets);
    return codegen_context;
}

#[no_mangle]
pub fn setup_masm() -> crate::isa::x64::masm::MacroAssembler {
    let isa_flags = crate::cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = crate::cranelift_codegen::settings::Flags::new();
    let ptr_size = 8;
    let masm_64 = crate::isa::x64::masm::MacroAssembler::new(ptr_size, shared_flags, isa_flags);
    return masm_64.unwrap();
}
