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
use seahorn_stubs::{assert, assume, error, nondet_i32, nondet_u32, nondet_u8};
use self::isa::x64::regs::{ALL_FPR, ALL_GPR, MAX_FPR, MAX_GPR, NON_ALLOCATABLE_FPR, NON_ALLOCATABLE_GPR};
use stack::Stack;
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
    let v: u32 = nondet_u32();
    match v {
        0 => masm_new(),
        1 => sub_ir(),
        2 => compile_function(),
        3 => checked_uadd(),
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
fn compile_function() {
    // let pointer_bytes = self.pointer_bytes();
    // let vmoffsets = VMOffsets::new(pointer_bytes, &translation.module);
    let vmoffsets = VMOffsets::new();

    // let mut body = body.get_binary_reader();
    // let mut masm = X64Masm::new(
    //     pointer_bytes,
    //     self.shared_flags.clone(),
    //     self.isa_flags.clone(),
    // )?;
    let stack = Stack::new();
    // TODO: We need a function signature that can be used for initializing
    // the ABI
    // let abi_sig = wasm_sig::<abi::X64ABI>(sig);

    // let env = FuncEnv::new(
    //     &vmoffsets,
    //     translation,
    //     types,
    //     builtins,
    //     self,
    //     abi::X64ABI::ptr_type(),
    // );
    // let type_converter = TypeConverter::new(env.translation, env.types);
    // let defined_locals =
    //     DefinedLocals::new::<abi::X64ABI>(&type_converter, &mut body, validator)?;
    // let frame = Frame::new::<abi::X64ABI>(&abi_sig, &defined_locals)?;
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
    let codegen_context = codegen::CodeGenContext::new(regalloc, stack, frame, &vmoffsets);
    assert(codegen_context.reachable);
    // let codegen = CodeGen::new(tunables, &mut masm, codegen_context, env, abi_sig);
}