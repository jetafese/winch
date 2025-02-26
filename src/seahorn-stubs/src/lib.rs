#![no_std]

extern "C" {
    fn __VERIFIER_error() -> !;
    fn __VERIFIER_assume(pred: i32);
    fn __VERIFIER_assert(pred: i32);
    fn __VERIFIER_nondet_u32() -> u32;
    fn __VERIFIER_nondet_u8() -> u8;
    fn __VERIFIER_nondet_i32() -> i32;
    fn __VERIFIER_nondet_i64() -> i64;
}

pub fn nondet_u32() -> u32 {
    unsafe {
        __VERIFIER_nondet_u32()
    }
}

pub fn nondet_u8() -> u8 {
    unsafe {
        __VERIFIER_nondet_u8()
    }
}

pub fn nondet_i32() -> i32 {
    unsafe {
        __VERIFIER_nondet_i32()
    }
}

pub fn nondet_i64() -> i64 {
    unsafe {
        __VERIFIER_nondet_i64()
    }
}

pub fn assume(pred: bool) {
    unsafe {
        __VERIFIER_assume(pred as i32)
    }
}

pub fn error() -> ! {
    unsafe {
        __VERIFIER_error()
    }
}

pub fn assert(pred: bool) {
    if !pred {
        error()
    }
}

