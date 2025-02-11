use crate::regalloc2::PReg;
pub use crate::regalloc2::RegClass;

/// A newtype abstraction on top of a physical register.
//
// NOTE
// This is temporary; the intention behind this newtype
// is to keep the usage of PReg contained to this module
// so that the rest of Winch should only need to operate
// on top of the concept of `Reg`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reg(PReg);

pub(crate) type WritableReg = crate::cranelift_codegen::Writable<Reg>;

// /// Mark a given register as writable. This macro constructs
// /// a [`cranelift_codegen::Writable`].
// macro_rules! writable {
//     ($e:expr) => {
//         crate::cranelift_codegen::Writable::from_reg($e)
//     };
// }

// pub(crate) use writable;

impl Reg {
    /// Create a register from its encoding and class.
    pub fn from(class: RegClass, enc: usize) -> Self {
        Self::new(PReg::new(enc, class))
    }

    /// Create a new register from a physical register.
    pub const fn new(raw: PReg) -> Self {
        Reg(raw)
    }

    /// Create a new general purpose register from encoding.
    pub fn int(enc: usize) -> Self {
        Self::new(PReg::new(enc, RegClass::Int))
    }

    /// Create a new floating point register from encoding.
    pub fn float(enc: usize) -> Self {
        Self::new(PReg::new(enc, RegClass::Float))
    }

    /// Get the encoding of the underlying register.
    pub const fn hw_enc(self) -> usize {
        self.0.hw_enc()
    }

    /// Get the physical register representation.
    pub(super) fn inner(&self) -> PReg {
        self.0
    }

    /// Get the register class.
    pub fn class(&self) -> RegClass {
        self.0.class()
    }

    /// Returns true if the registers is a general purpose
    /// integer register.
    pub fn is_int(&self) -> bool {
        self.class() == RegClass::Int
    }

    /// Returns true if the registers is a float register.
    pub fn is_float(&self) -> bool {
        self.class() == RegClass::Float
    }
}

impl From<Reg> for crate::cranelift_codegen::Reg {
    fn from(reg: Reg) -> Self {
        reg.inner().into()
    }
}

impl std::convert::From<crate::regalloc2::PReg> for crate::cranelift_codegen::Reg {
    fn from(preg: crate::regalloc2::PReg) -> crate::cranelift_codegen::Reg {
        crate::cranelift_codegen::RealReg(preg).into()
    }
}

impl std::convert::From<crate::cranelift_codegen::RealReg> for crate::cranelift_codegen::Reg {
    fn from(reg: crate::cranelift_codegen::RealReg) -> crate::cranelift_codegen::Reg {
        crate::cranelift_codegen::Reg(reg.into())
    }
}

impl std::convert::From<crate::cranelift_codegen::RealReg> for crate::cranelift_codegen::VReg {
    fn from(reg: crate::cranelift_codegen::RealReg) -> crate::cranelift_codegen::VReg {
        // This representation is redundant: the class is implied in the vreg
        // index as well as being in the vreg class field.
        crate::cranelift_codegen::VReg::new(reg.0.index(), reg.0.class())
    }
}

impl std::convert::From<crate::cranelift_codegen::RealReg> for crate::regalloc2::PReg {
    fn from(reg: crate::cranelift_codegen::RealReg) -> crate::regalloc2::PReg {
        reg.0
    }
}

impl std::fmt::Debug for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", 0)
    }
}
