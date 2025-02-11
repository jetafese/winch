// #![no_std]

#![no_builtins]
// #![no_std]
#![no_main]
// #![feature(default_alloc_error_handler)]

use cranelift_codegen::{Reg, VReg, Writable};

pub mod isa;
// pub use isa::*;
mod masm;
mod regalloc;
// mod regset;
// mod stack;
// mod visitor;
/* stubbed libraries */
mod cranelift_codegen;
// mod wasmtime_environ;
mod regalloc2;

// use seahorn_stubs::assert;

extern "C" {
    fn __VERIFIER_error() -> !;
    fn __VERIFIER_assume(pred: i32);
    fn __VERIFIER_nondet_u32() -> u32;
}

// use core::panic::PanicInfo;

// #[panic_handler]
// #[no_mangle]
// fn panic(_panic: &PanicInfo<'_>) -> ! {
//     unsafe {
//         __VERIFIER_error();
//     }
// }

fn nondet_u32() -> u32 {
    unsafe {
        __VERIFIER_nondet_u32()
    }
}

fn error() -> ! {
    unsafe {
        __VERIFIER_error()
    }
}

#[allow(unused)]
#[no_mangle]
pub extern fn main(_argc: i32, _argv: *const *const u8) -> u32 {
    let isa_flags = cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = cranelift_codegen::settings::Flags::new();
    let mut asm = isa::x64::asm::Assembler::new(shared_flags, isa_flags);
    let src = Reg(VReg::new(32, regalloc2::RegClass::Int));
    let dst = isa::x64::regs::scratch();
    asm.add_rr(src, Writable::from_reg(dst),masm::OperandSize::S32);
    // assert!(asm.finalize(None).srclocs)
    // assert(2 > 5);
    error();
    nondet_u32()
}