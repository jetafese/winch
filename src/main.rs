//! Code generation library for Winch.

// #![expect(clippy::allow_attributes_without_reason, reason = "crate not migrated")]

// Unless this library is compiled with `all-arch`, the rust compiler
// is going to emit dead code warnings. This directive is fine as long
// as we configure to run CI at least once with the `all-arch` feature
// enabled.
// #![cfg_attr(not(feature = "all-arch"), allow(dead_code))]

// // mod abi;
// // pub use codegen::{BuiltinFunctions, FuncEnv};
// // mod codegen;
// // mod frame;
// pub mod isa;
// // pub use isa::*;
// mod masm;
// mod regalloc;
// // mod regset;
// // mod stack;
// // mod visitor;
// /* stubbed libraries */
// mod cranelift_codegen;
// // mod wasmtime_environ;
// mod regalloc2;
// // mod wasmparser;
// // mod wasmtime_cranelift;
// // mod target_lexicon;

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

use cranelift_codegen::Writable;
// use isa::reg::{self, Reg};
// use regalloc2::PReg;
use regset::RegBitSet;
use seahorn_stubs::{assert, assume, error, nondet_i32, nondet_u32, nondet_u8};
use self::isa::x64::regs::{ALL_FPR, ALL_GPR, MAX_FPR, MAX_GPR, NON_ALLOCATABLE_FPR, NON_ALLOCATABLE_GPR};
use stack::Stack;
use frame::Frame;

// extern "C" {
//     fn __VERIFIER_error() -> !;
// //     fn __VERIFIER_assume(pred: i32);
// //     fn __VERIFIER_nondet_u32() -> u32;
// }

use core::panic::PanicInfo;

#[panic_handler]
#[no_mangle]
fn panic(_panic: &PanicInfo<'_>) -> ! {
    error();
}

// fn nondet_u32() -> u32 {
//     unsafe {
//         __VERIFIER_nondet_u32()
//     }
// }

// fn error() -> ! {
//     unsafe {
//         __VERIFIER_error()
//     }
// }

#[allow(unused)]
#[no_mangle]
pub extern fn main() {
    // let v: u32 = nondet_u32();
    // match v {
    //     0 => checked_uadd(),
    //     // 1 => sub_ir(),
    //     2 => compile_function(),
    //     _ => (),
    // }
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
    // let codegen_context = codegen::CodeGenContext::new(regalloc, stack, frame, &vmoffsets);
    let codegen_context = codegen::CodeGenContext::new(regalloc, stack, frame);
    // let codegen = CodeGen::new(tunables, &mut masm, codegen_context, env, abi_sig);
    assert(true);
}

#[no_mangle]
fn checked_uadd() {
    assume(true);
    let isa_flags = cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = cranelift_codegen::settings::Flags::new();
    let ptr_size = nondet_u8();
    let masm_64 = isa::x64::masm::MacroAssembler::new(ptr_size, shared_flags, isa_flags);
    masm_64.err();
    // let mut asm = isa::x64::asm::Assembler::new(shared_flags, isa_flags);
    // let src = Reg(PReg::new(2, regalloc2::RegClass::Int));
    // let dst = isa::x64::regs::scratch();
    // asm.add_rr(src, Writable::from_reg(dst),masm::OperandSize::S32);
    assert(true);
}

#[no_mangle]
fn sub_ir() {
    let isa_flags = cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = cranelift_codegen::settings::Flags::new();
    let mut asm = isa::x64::asm::Assembler::new(shared_flags, isa_flags);
    let src = nondet_i32();
    let dst = isa::x64::regs::scratch();
    asm.sub_ir(src, Writable::from_reg(dst),masm::OperandSize::S32);
    assert(true);
}

// #[no_mangle]
// fn compile_function() {
//     // let pointer_bytes = self.pointer_bytes();
//     // let vmoffsets = VMOffsets::new(pointer_bytes, &translation.module);

//     // let mut body = body.get_binary_reader();
//     // let mut masm = X64Masm::new(
//     //     pointer_bytes,
//     //     self.shared_flags.clone(),
//     //     self.isa_flags.clone(),
//     // )?;
//     let stack = Stack::new();
//     // TODO: We need a function signature that can be used for initializing
//     // the ABI
//     // let abi_sig = wasm_sig::<abi::X64ABI>(sig);

//     // let env = FuncEnv::new(
//     //     &vmoffsets,
//     //     translation,
//     //     types,
//     //     builtins,
//     //     self,
//     //     abi::X64ABI::ptr_type(),
//     // );
//     // let type_converter = TypeConverter::new(env.translation, env.types);
//     // let defined_locals =
//     //     DefinedLocals::new::<abi::X64ABI>(&type_converter, &mut body, validator)?;
//     // let frame = Frame::new::<abi::X64ABI>(&abi_sig, &defined_locals)?;
//     let frame = Frame::new()?;

//     let gpr = RegBitSet::int(
//         ALL_GPR.into(),
//         NON_ALLOCATABLE_GPR.into(),
//         usize::try_from(MAX_GPR).unwrap(),
//     );
//     let fpr = RegBitSet::float(
//         ALL_FPR.into(),
//         NON_ALLOCATABLE_FPR.into(),
//         usize::try_from(MAX_FPR).unwrap(),
//     );
//     let regalloc = regalloc::RegAlloc::from(gpr, fpr);
//     // let codegen_context = codegen::CodeGenContext::new(regalloc, stack, frame, &vmoffsets);
//     let codegen_context = codegen::CodeGenContext::new(regalloc, stack, frame);
//     // let codegen = CodeGen::new(tunables, &mut masm, codegen_context, env, abi_sig);
//     assert(true);
// }