#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Triple {}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Architecture {
    Unknown,
    Arm(),
    AmdGcn,
    Aarch64(),
    Asmjs,
    Avr,
    Bpfeb,
    Bpfel,
    Hexagon,
    X86_32(),
    M68k,
    LoongArch64,
    Mips32(),
    Mips64(),
    Msp430,
    Nvptx64,
    Pulley32,
    Pulley64,
    Pulley32be,
    Pulley64be,
    Powerpc,
    Powerpc64,
    Powerpc64le,
    Riscv32(),
    Riscv64(),
    S390x,
    Sparc,
    Sparc64,
    Sparcv9,
    Wasm32,
    Wasm64,
    X86_64,
    /// x86_64 target that only supports Haswell-compatible Intel chips.
    X86_64h,
    XTensa,
    Clever()
}