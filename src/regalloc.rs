#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PReg {
    bits: u8,
}

impl PReg {
    pub const MAX_BITS: usize = 6;
    pub const MAX: usize = (1 << Self::MAX_BITS) - 1;
    pub const NUM_INDEX: usize = 1 << (Self::MAX_BITS + 2); // including RegClass bits

    /// Create a new PReg. The `hw_enc` range is 6 bits.
    #[inline(always)]
    pub const fn new(hw_enc: usize, class: RegClass) -> Self {
        debug_assert!(hw_enc <= PReg::MAX);
        PReg {
            bits: ((class as u8) << Self::MAX_BITS) | (hw_enc as u8),
        }
    }

    /// The physical register number, as encoded by the ISA for the particular register class.
    #[inline(always)]
    pub const fn hw_enc(self) -> usize {
        self.bits as usize & Self::MAX
    }

    /// The register class.
    #[inline(always)]
    pub const fn class(self) -> RegClass {
        match (self.bits >> Self::MAX_BITS) & 0b11 {
            0 => RegClass::Int,
            1 => RegClass::Float,
            2 => RegClass::Vector,
            _ => unreachable!(),
        }
    }

    /// Get an index into the (not necessarily contiguous) index space of
    /// all physical registers. Allows one to maintain an array of data for
    /// all PRegs and index it efficiently.
    #[inline(always)]
    pub const fn index(self) -> usize {
        self.bits as usize
    }

    /// Construct a PReg from the value returned from `.index()`.
    #[inline(always)]
    pub const fn from_index(index: usize) -> Self {
        PReg {
            bits: (index & (Self::NUM_INDEX - 1)) as u8,
        }
    }

    /// Return the "invalid PReg", which can be used to initialize
    /// data structures.
    #[inline(always)]
    pub const fn invalid() -> Self {
        PReg::new(Self::MAX, RegClass::Int)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegClass {
    Int = 0,
    Float = 1,
    Vector = 2,
}

// use crate::{
//     codegen::CodeGenError,
//     isa::reg::{Reg, RegClass},
//     regset::{RegBitSet, RegSet},
// };

// use anyhow::{anyhow, Result};

// /// The register allocator.
// ///
// /// The register allocator uses a single-pass algorithm;
// /// its implementation uses a bitset as a freelist
// /// to track per-class register availability.
// ///
// /// If a particular register is not available upon request
// /// the register allocation will perform a "spill", essentially
// /// moving Local and Register values in the stack to memory.
// /// This process ensures that whenever a register is requested,
// /// it is going to be available.
// pub(crate) struct RegAlloc {
//     /// The register set.
//     regset: RegSet,
// }

// impl RegAlloc {
//     /// Create a register allocator from a bit set for each register class.
//     pub fn from(gpr: RegBitSet, fpr: RegBitSet) -> Self {
//         let rs = RegSet::new(gpr, fpr);
//         Self { regset: rs }
//     }

//     /// Allocate the next available register for the given class,
//     /// spilling if not available.
//     pub fn reg_for_class<F>(&mut self, class: RegClass, spill: &mut F) -> Result<Reg>
//     where
//         F: FnMut(&mut RegAlloc) -> Result<()>,
//     {
//         match self.regset.reg_for_class(class) {
//             Some(reg) => Ok(reg),
//             None => {
//                 spill(self)?;
//                 self.regset
//                     .reg_for_class(class)
//                     .ok_or_else(|| anyhow!(CodeGenError::expected_register_to_be_available()))
//             }
//         }
//     }

//     /// Returns true if the specified register is allocatable.
//     pub fn reg_available(&self, reg: Reg) -> bool {
//         self.regset.named_reg_available(reg)
//     }

//     /// Request a specific register, spilling if not available.
//     pub fn reg<F>(&mut self, named: Reg, mut spill: F) -> Result<Reg>
//     where
//         F: FnMut(&mut RegAlloc) -> Result<()>,
//     {
//         match self.regset.reg(named) {
//             Some(reg) => Ok(reg),
//             None => {
//                 spill(self)?;
//                 self.regset
//                     .reg(named)
//                     .ok_or_else(|| anyhow!(CodeGenError::expected_register_to_be_available()))
//             }
//         }
//     }

//     /// Free the given register.
//     pub fn free(&mut self, reg: Reg) {
//         self.regset.free(reg);
//     }
// }
