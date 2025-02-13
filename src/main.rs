#![no_builtins]
#![no_std]
#![no_main]

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

use cranelift_codegen::Writable;
use isa::reg::Reg;
use regalloc2::PReg;
use seahorn_stubs::{assert, assume, error, nondet_i32, nondet_u32};

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
    let v: u32 = nondet_u32();
    match v {
        0 => add_rr(),
        1 => sub_ir(),
        _ => (),
    }
}

#[no_mangle]
fn add_rr() {
    assume(true);
    let isa_flags = cranelift_codegen::isa::x64::x64_settings::Flags::new();
    let shared_flags = cranelift_codegen::settings::Flags::new();
    let mut asm = isa::x64::asm::Assembler::new(shared_flags, isa_flags);
    let src = Reg(PReg::new(2, regalloc2::RegClass::Int));
    let dst = isa::x64::regs::scratch();
    asm.add_rr(src, Writable::from_reg(dst),masm::OperandSize::S32);
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