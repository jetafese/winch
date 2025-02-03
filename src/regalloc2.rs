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