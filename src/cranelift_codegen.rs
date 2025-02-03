/// Out-of-line data for calls, to keep the size of `Inst` down.
#[derive(Clone, Debug)]
pub struct CallInfo<T> {
    /// Receiver of this call
    pub dest: T,
    /// The calling convention of the callee.
    pub callee_conv: isa::CallConv,
    /// The calling convention of the caller.
    pub caller_conv: isa::CallConv,
    /// The number of bytes that the callee will pop from the stack for the
    /// caller, if any. (Used for popping stack arguments with the `tail`
    /// calling convention.)
    pub callee_pop_size: u32,
}

impl<T> CallInfo<T> {
    /// Creates an empty set of info with no clobbers/uses/etc with the
    /// specified ABI
    pub fn empty(dest: T, call_conv: isa::CallConv) -> CallInfo<T> {
        CallInfo {
            dest,
            caller_conv: call_conv,
            callee_conv: call_conv,
            callee_pop_size: 0,
        }
    }

    /// Change the `T` payload on this info to `U`.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> CallInfo<U> {
        CallInfo {
            dest: f(self.dest),
            caller_conv: self.caller_conv,
            callee_conv: self.callee_conv,
            callee_pop_size: self.callee_pop_size,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MachLabel(u32);

pub trait CompilePhase {
    type MachSrcLocType: for<'a> core::fmt::Debug + PartialEq + Clone;
    type SourceLocType: for<'a> core::fmt::Debug + PartialEq + Clone;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stencil;

/// Status of a compiled artifact ready to use.
#[derive(Clone, Debug, PartialEq)]
pub struct Final;

pub trait VCodeInst: MachInst + MachInstEmit {}
impl<I: MachInst + MachInstEmit> VCodeInst for I {}

pub trait MachInst: Clone + std::fmt::Debug {
    type LabelUse: MachInstLabelUse;
}

pub trait MachInstLabelUse: Clone + Copy + std::fmt::Debug + Eq {}
pub trait MachInstEmit: MachInst {
    /// Persistent state carried across `emit` invocations.
    type State: MachInstEmitState<Self>;

    /// Constant information used in `emit` invocations.
    type Info;

    /// Emit the instruction.
    fn emit(&self, code: &mut MachBuffer<Self>, info: &Self::Info, state: &mut Self::State);

    /// Pretty-print the instruction.
    fn pretty_print_inst(&self, state: &mut Self::State) -> String;
}

pub trait MachInstEmitState<I: VCodeInst>: Default + Clone + std::fmt::Debug {}

pub struct MachBuffer<I: VCodeInst> {
    kind: I::LabelUse,
}

pub struct MachBufferFinalized<T: CompilePhase> {
    pub(crate) srclocs: T::MachSrcLocType,
}

pub mod ir {
    pub mod types {
        #[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
        pub struct Type(u16);
        pub const I64: Type = Type(0x77);
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
    pub struct TrapCode(core::num::NonZeroU8);

    impl TrapCode {
        /// Number of reserved opcodes for Cranelift itself. This number of traps are
        /// defined below starting at the high end of the byte space (e.g. 255, 254,
        /// ...)
        const RESERVED: u8 = 5;
        const RESERVED_START: u8 = u8::MAX - Self::RESERVED + 1;
    
        /// Internal helper to create new reserved trap codes.
        const fn reserved(byte: u8) -> TrapCode {
            if let Some(code) = byte.checked_add(Self::RESERVED_START) {
                if let Some(nz) = core::num::NonZeroU8::new(code) {
                    return TrapCode(nz);
                }
            }
            panic!("invalid reserved opcode")
        }
    
        pub const fn as_raw(&self) -> core::num::NonZeroU8 {
            self.0
        }

        /// The current stack space was exhausted.
        pub const STACK_OVERFLOW: TrapCode = TrapCode::reserved(0);
        /// An integer arithmetic operation caused an overflow.
        pub const INTEGER_OVERFLOW: TrapCode = TrapCode::reserved(1);
        /// A `heap_addr` instruction detected an out-of-bounds error.
        ///
        /// Note that not all out-of-bounds heap accesses are reported this way;
        /// some are detected by a segmentation fault on the heap unmapped or
        /// offset-guard pages.
        pub const HEAP_OUT_OF_BOUNDS: TrapCode = TrapCode::reserved(2);
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct SourceLoc(u32);

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct RelSourceLoc(u32);

    const BIT_LITTLE_ENDIAN: u16 = 1 << 2;
    const BIT_BIG_ENDIAN: u16 = 1 << 3;
    const BIT_ALIGNED: u16 = 1 << 0;
    
    const MASK_TRAP_CODE: u16 = 0b1111_1111 << TRAP_CODE_OFFSET;
    const TRAP_CODE_OFFSET: u16 = 7;

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
    pub enum Endianness {
        Little,
        Big,
    }

    #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
    pub struct MemFlags {
        bits: u16,
    }
    impl MemFlags {
        /// Create a new empty set of flags.
        pub const fn new() -> Self {
            Self { bits: 0 }.with_trap_code(Some(TrapCode::HEAP_OUT_OF_BOUNDS))
        }
    
        pub const fn with_trap_code(mut self, code: Option<TrapCode>) -> Self {
            let bits = match code {
                Some(code) => code.as_raw().get() as u16,
                None => 0,
            };
            self.bits &= !MASK_TRAP_CODE;
            self.bits |= bits << TRAP_CODE_OFFSET;
            self
        }

        /// Create a set of flags representing an access from a "trusted" address, meaning it's
        /// known to be aligned and non-trapping.
        pub const fn trusted() -> Self {
            Self::new().with_notrap().with_aligned()
        }

        pub const fn with_notrap(self) -> Self {
            self.with_trap_code(None)
        }

        const fn with_bit(mut self, bit: u16) -> Self {
            self.bits |= bit;
            self
        }

        pub const fn with_endianness(self, endianness: Endianness) -> Self {
            let res = match endianness {
                Endianness::Little => self.with_bit(BIT_LITTLE_ENDIAN),
                Endianness::Big => self.with_bit(BIT_BIG_ENDIAN),
            };
            assert!(!(res.read_bit(BIT_LITTLE_ENDIAN) && res.read_bit(BIT_BIG_ENDIAN)));
            res
        }

        const fn read_bit(self, bit: u16) -> bool {
            self.bits & bit != 0
        }

        pub const fn with_aligned(self) -> Self {
            self.with_bit(BIT_ALIGNED)
        }
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub enum LibCall {
        /// probe for stack overflow. These are emitted for functions which need
        /// when the `enable_probestack` setting is true.
        Probestack,
        /// ceil.f32
        CeilF32,
        /// ceil.f64
        CeilF64,
        /// floor.f32
        FloorF32,
        /// floor.f64
        FloorF64,
        /// trunc.f32
        TruncF32,
        /// frunc.f64
        TruncF64,
        /// nearest.f32
        NearestF32,
        /// nearest.f64
        NearestF64,
        /// fma.f32
        FmaF32,
        /// fma.f64
        FmaF64,
        /// libc.memcpy
        Memcpy,
        /// libc.memset
        Memset,
        /// libc.memmove
        Memmove,
        /// libc.memcmp
        Memcmp,
    
        /// Elf __tls_get_addr
        ElfTlsGetAddr,
        /// Elf __tls_get_offset
        ElfTlsGetOffset,
    
        /// The `pshufb` on x86 when SSSE3 isn't available.
        X86Pshufb,
        // When adding a new variant make sure to add it to `all_libcalls` too.
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
    pub struct UserExternalNameRef(u32);

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum ExternalName {
        /// A reference to a name in a user-defined symbol table.
        User(UserExternalNameRef),
        /// A test case function name of up to a hardcoded amount of ascii
        /// characters. This is not intended to be used outside test cases.
        TestCase(),
        /// A well-known runtime library function.
        LibCall(LibCall),
        /// A well-known symbol.
        KnownSymbol(),
    }
}

pub mod settings {
    /// Represents the kind of setting.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub enum SettingKind {
        /// The setting is an enumeration.
        Enum,
        /// The setting is a number.
        Num,
        /// The setting is a boolean.
        Bool,
        /// The setting is a preset.
        Preset,
    }
    
    #[derive(Clone, Hash)]
    /// Flags group `shared`.
    pub struct Flags {
        bytes: [u8; 11],
    }

    /// Represents an available builder setting.
    ///
    /// This is used for iterating settings in a builder.
    #[derive(Clone, Copy, Debug)]
    pub struct Setting {
        /// The name of the setting.
        pub name: &'static str,
        /// The description of the setting.
        pub description: &'static str,
        /// The kind of the setting.
        pub kind: SettingKind,
        /// The supported values of the setting (for enum values).
        pub values: Option<&'static [&'static str]>,
    }
}

pub mod isa {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub enum CallConv {
        Fast,
        Cold,
        Tail,
        SystemV,
        WindowsFastcall,
        AppleAarch64,
        Probestack,
        Winch,
    }
    pub mod aarch64 {
        pub mod inst {
            pub mod emit {
                pub struct EmitInfo(crate::cranelift_codegen::settings::Flags);

                #[derive(Default, Clone, Debug)]
                pub struct EmitState {}
            }

            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            pub enum OperandSize {
                /// 32-bit.
                Size32,
                /// 64-bit.
                Size64,
            }

            #[derive(Clone, Copy, Debug)]
            pub struct NZCV {
                /// The negative condition flag.
                n: bool,
                /// The zero condition flag.
                z: bool,
                /// The carry condition flag.
                c: bool,
                /// The overflow condition flag.
                v: bool,
            }

            #[derive(Clone, Copy, Debug)]
            pub struct UImm5 {
                value: u8,
            }

            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            pub enum VectorSize {
                Size8x8,
                Size8x16,
                Size16x4,
                Size16x8,
                Size32x2,
                Size32x4,
                Size64x2,
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum VecLanesOp {
                Addv,
                Uminv,
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum VecMisc2 {
                Not,
                Neg,
                Abs
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum IntToFpuOp {
                U32ToF32,
                I32ToF32,
                U32ToF64,
                I32ToF64,
                U64ToF32,
                I64ToF32,
                U64ToF64,
                I64ToF64,
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum FpuToIntOp {
                F32ToU32,
                F32ToI32,
                F32ToU64,
                F32ToI64,
                F64ToU32,
                F64ToI32,
                F64ToU64,
                F64ToI64,
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum ALUOp {
                Add,
                Sub,
                Orr,
                OrrNot,
                And,
                AndS,
                AndNot,
                Eor,
                EorNot,
                AddS,
                SubS,
                SMulH,
                UMulH,
                SDiv,
                UDiv,
                RotR,
                Lsr,
                Asr,
                Lsl,
                Adc,
                AdcS,
                Sbc,
                SbcS,
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum ALUOp3 {
                MAdd,
                MSub,
                UMAddL,
                SMAddL,
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum BitOp {
                RBit,
                Clz,
                Cls,
                Rev16,
                Rev32,
                Rev64,
            }

            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            pub enum BranchTarget {
                /// An unresolved reference to a Label, as passed into
                /// `lower_branch_group()`.
                Label(crate::cranelift_codegen::MachLabel),
                /// A fixed PC offset.
                ResolvedOffset(i32),
            }

            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            #[repr(u8)]
            pub enum Cond {
                /// Equal.
                Eq = 0,
                /// Not equal.
                Ne = 1,
                /// Unsigned greater than or equal to.
                Hs = 2,
                /// Unsigned less than.
                Lo = 3,
                /// Minus, negative.
                Mi = 4,
                /// Positive or zero.
                Pl = 5,
                /// Signed overflow.
                Vs = 6,
                /// No signed overflow.
                Vc = 7,
                /// Unsigned greater than.
                Hi = 8,
                /// Unsigned less than or equal to.
                Ls = 9,
                /// Signed greater or equal to.
                Ge = 10,
                /// Signed less than.
                Lt = 11,
                /// Signed greater than.
                Gt = 12,
                /// Signed less than or equal.
                Le = 13,
                /// Always executed.
                Al = 14,
                /// Always executed.
                Nv = 15,
            }

            #[derive(Clone, Debug)]
            pub enum CondBrKind {
                /// Condition: given register is zero.
                Zero(crate::isa::reg::Reg, crate::masm::OperandSize),
                /// Condition: given register is nonzero.
                NotZero(crate::isa::reg::Reg, crate::masm::OperandSize),
                /// Condition: the given condition-code test is true.
                Cond(Cond),
            }

            #[derive(Clone, Copy, Debug)]
            #[repr(u8)]
            pub enum ExtendOp {
                /// Unsigned extend byte.
                UXTB = 0b000,
                /// Unsigned extend halfword.
                UXTH = 0b001,
                /// Unsigned extend word.
                UXTW = 0b010,
                /// Unsigned extend doubleword.
                UXTX = 0b011,
                /// Signed extend byte.
                SXTB = 0b100,
                /// Signed extend halfword.
                SXTH = 0b101,
                /// Signed extend word.
                SXTW = 0b110,
                /// Signed extend doubleword.
                SXTX = 0b111,
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum FPUOp1 {
                Abs,
                Neg,
                Sqrt,
                Cvt32To64,
                Cvt64To32,
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum FPUOp2 {
                Add,
                Sub,
                Mul,
                Div,
                Max,
                Min,
            }

            #[derive(Copy, Clone, Debug)]
            pub enum FPUOpRI {
                /// Unsigned right shift. Rd = Rn << #imm
                UShr32(FPURightShiftImm),
                /// Unsigned right shift. Rd = Rn << #imm
                UShr64(FPURightShiftImm),
            }

            #[derive(Clone, Copy, Debug)]
            pub struct FPULeftShiftImm {
                /// Shift amount.
                pub amount: u8,
                /// Lane size in bits.
                pub lane_size_in_bits: u8,
            }

            #[derive(Clone, Copy, Debug)]
            pub struct FPURightShiftImm {
                /// Shift amount.
                pub amount: u8,
                /// Lane size in bits.
                pub lane_size_in_bits: u8,
            }

            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
            pub enum FpuRoundMode {
                Minus32,
                Minus64,
                Plus32,
                Plus64,
                Zero32,
                Zero64,
                Nearest32,
                Nearest64,
            }

            #[derive(Copy, Clone, Debug, PartialEq)]
            pub struct ImmLogic {
                /// The actual value.
                value: u64,
                /// `N` flag.
                pub n: bool,
                /// `S` field: element size and element bits.
                pub r: u8,
                /// `R` field: rotate amount.
                pub s: u8,
                /// Was this constructed for a 32-bit or 64-bit instruction?
                pub size: crate::masm::OperandSize,
            }

            #[derive(Copy, Clone, Debug)]
            pub struct ImmShift {
                /// 6-bit shift amount.
                pub imm: u8,
            }

            #[derive(Copy, Clone, Debug)]
            pub struct Imm12 {
                /// The immediate bits.
                pub bits: u16,
                /// Whether the immediate bits are shifted left by 12 or not.
                pub shift12: bool,
            }

            #[derive(Copy, Clone, Debug)]
            pub enum FPUOpRIMod {
                /// Shift left and insert. Rd |= Rn << #imm
                Sli32(FPULeftShiftImm),
                /// Shift left and insert. Rd |= Rn << #imm
                Sli64(FPULeftShiftImm),
            }

            #[derive(Clone, Copy, Debug, PartialEq, Eq)]
            pub enum ScalarSize {
                Size8,
                Size16,
                Size32,
                Size64,
                Size128,
            }

            #[derive(Clone, Copy, Debug, PartialEq)]
            pub struct ASIMDFPModImm {
                imm: u8,
                size: ScalarSize,
            }

            #[derive(Clone, Debug)]
            pub enum AMode {
                SPPreIndexed {
                    simm9: SImm9,
                },
                SPPostIndexed {
                    simm9: SImm9,
                },
                RegOffset {
                    // rn: crate::cranelift_codegen::Reg,
                    off: i64,
                },
            }

            #[derive(Clone, Debug)]
            pub enum MInst {
                Nop0,
                Nop4,
            }

            #[derive(Clone, Debug)]
            pub enum PairAMode {
                SignedOffset {
                    reg: crate::cranelift_codegen::Reg,
                    simm7: SImm7Scaled,
                },
                SPPreIndexed {
                    simm7: SImm7Scaled,
                },
                SPPostIndexed {
                    simm7: SImm7Scaled,
                },
            }

            #[derive(Clone, Copy, Debug)]
            pub struct SImm7Scaled {
                /// The value.
                pub value: i16,
                /// multiplied by the size of this type
                pub scale_ty: crate::cranelift_codegen::ir::types::Type,
            }
            impl SImm7Scaled {
                /// Create a SImm7Scaled from a raw offset and the known scale type, if
                /// possible.
                pub fn maybe_from_i64(value: i64, scale_ty: crate::cranelift_codegen::ir::types::Type) -> Option<SImm7Scaled> {
                    // assert!(scale_ty == I64 || scale_ty == I32 || scale_ty == F64 || scale_ty == I8X16);
                    // let scale = scale_ty.bytes();
                    let scale: u32 = 64;
                    assert!(scale.is_power_of_two());
                    let scale = i64::from(scale);
                    let upper_limit = 63 * scale;
                    let lower_limit = -(64 * scale);
                    if value >= lower_limit && value <= upper_limit && (value & (scale - 1)) == 0 {
                        Some(SImm7Scaled {
                            value: i16::try_from(value).unwrap(),
                            scale_ty,
                        })
                    } else {
                        None
                    }
                }
            
                /// Bits for encoding.
                pub fn bits(&self) -> u32 {
                    // let ty_bytes: i16 = self.scale_ty.bytes() as i16;
                    let ty_bytes: i16 = 64 as i16;
                    let scaled: i16 = self.value / ty_bytes;
                    assert!(scaled <= 63 && scaled >= -64);
                    let scaled: i8 = scaled as i8;
                    let encoded: u32 = scaled as u32;
                    encoded & 0x7f
                }
            }

            #[derive(Clone, Copy, Debug)]
            pub struct SImm9 {
                /// The value.
                pub value: i16,
            }
            impl SImm9 {
                /// Create a signed 9-bit offset from a full-range value, if possible.
                pub fn maybe_from_i64(value: i64) -> Option<SImm9> {
                    if value >= -256 && value <= 255 {
                        Some(SImm9 {
                            value: value as i16,
                        })
                    } else {
                        None
                    }
                }
            
                /// Bits for encoding.
                pub fn bits(&self) -> u32 {
                    (self.value as u32) & 0x1ff
                }
            
                /// Signed value of immediate.
                pub fn value(&self) -> i32 {
                    self.value as i32
                }
            }
        }
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Writable<T> {
    reg: T,
}
impl<T> Writable<T> {
    /// Explicitly construct a `Writable<T>` from a `T`. As noted in
    /// the documentation for `Writable`, this is not hidden or
    /// disallowed from the outside; anyone can perform the "cast";
    /// but it is explicit so that we can audit the use sites.
    pub fn from_reg(reg: T) -> Writable<T> {
        Writable { reg }
    }

    /// Get the underlying register, which can be read.
    pub fn to_reg(self) -> T {
        self.reg
    }

    /// Get a mutable borrow of the underlying register.
    pub fn reg_mut(&mut self) -> &mut T {
        &mut self.reg
    }

    /// Map the underlying register to another value or type.
    pub fn map<U>(self, f: impl Fn(T) -> U) -> Writable<U> {
        Writable { reg: f(self.reg) }
    }
}

pub type RegClass = crate::regalloc2::RegClass;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VReg {
    bits: u32,
}

impl VReg {
    pub const MAX_BITS: usize = 21;
    pub const MAX: usize = (1 << Self::MAX_BITS) - 1;

    #[inline(always)]
    pub const fn new(virt_reg: usize, class: RegClass) -> Self {
        debug_assert!(virt_reg <= VReg::MAX);
        VReg {
            bits: ((virt_reg as u32) << 2) | (class as u8 as u32),
        }
    }

    #[inline(always)]
    pub const fn vreg(self) -> usize {
        let vreg = (self.bits >> 2) as usize;
        vreg
    }

    #[inline(always)]
    pub const fn class(self) -> RegClass {
        match self.bits & 0b11 {
            0 => RegClass::Int,
            1 => RegClass::Float,
            2 => RegClass::Vector,
            _ => unreachable!(),
        }
    }

    #[inline(always)]
    pub const fn invalid() -> Self {
        VReg::new(Self::MAX, RegClass::Int)
    }

    #[inline(always)]
    pub const fn bits(self) -> usize {
        self.bits as usize
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RealReg(crate::regalloc2::PReg);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VirtualReg(VReg);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reg(VReg);
impl Reg {
    /// Get the physical register (`RealReg`), if this register is
    /// one.
    pub fn to_real_reg(self) -> Option<RealReg> {
        None
    }

    /// Get the virtual (non-physical) register, if this register is
    /// one.
    pub fn to_virtual_reg(self) -> Option<VirtualReg> {
        None
    }

    /// Get the class of this register.
    pub fn class(self) -> RegClass {
        self.0.class()
    }

    /// Is this a real (physical) reg?
    pub fn is_real(self) -> bool {
        self.to_real_reg().is_some()
    }

    /// Is this a virtual reg?
    pub fn is_virtual(self) -> bool {
        self.to_virtual_reg().is_some()
    }
}

impl std::fmt::Debug for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.0 == VReg::invalid() {
            write!(f, "<invalid>")
            // } else if let Some(rreg) = self.to_real_reg() {
                // let preg: crate::regalloc2::PReg = rreg.into();
            // } else if let Some(vreg) = self.to_virtual_reg() {
                    // let vreg: VReg = vreg.into();
        } else if let Some(_rreg) = self.to_real_reg() {
            write!(f, "preg")
        } else if let Some(_vreg) = self.to_virtual_reg() {
            write!(f, "vreg")
        } else {
            unreachable!()
        }
    }
}

// pub mod binemit {
//     pub type CodeOffset = u32;
// }

// #[derive(Clone, Debug)]
// pub struct CallInfo<T> {}
// impl<T> CallInfo<T> {
//     pub fn empty(dest: T, call_conv: isa::CallConv) -> CallInfo<T>{}
// }

// pub trait MachInst: Clone {}
// pub trait MachInstEmit: MachInst {}
// #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct MachLabel(u32);

// pub trait VCodeInst: MachInst + MachInstEmit {}
// impl<I: MachInst + MachInstEmit> VCodeInst for I {}

// pub trait MachInstEmitState<I: VCodeInst>: Default + Clone {}
// pub struct MachBuffer<I: VCodeInst> {}
// impl<I: VCodeInst> MachBuffer<I> {}

// pub mod settings {
//     /// Represents the kind of setting.
//     #[derive(Clone, Copy, Debug, Eq, PartialEq)]
//     pub enum SettingKind {
//         /// The setting is an enumeration.
//         Enum,
//         /// The setting is a number.
//         Num,
//         /// The setting is a boolean.
//         Bool,
//         /// The setting is a preset.
//         Preset,
//     }
    
//     #[derive(Clone, Hash)]
//     /// Flags group `shared`.
//     pub struct Flags {
//         bytes: [u8; 11],
//     }

//     /// Represents an available builder setting.
//     ///
//     /// This is used for iterating settings in a builder.
//     #[derive(Clone, Copy, Debug)]
//     pub struct Setting {
//         /// The name of the setting.
//         pub name: &'static str,
//         /// The description of the setting.
//         pub description: &'static str,
//         /// The kind of the setting.
//         pub kind: SettingKind,
//         /// The supported values of the setting (for enum values).
//         pub values: Option<&'static [&'static str]>,
//     }
// }

// pub trait CompilePhase {}

// #[derive(Clone, Debug, PartialEq)]
// pub struct Final;

// pub struct MachBufferFinalized<T: CompilePhase> {}

// pub trait TextSectionBuilder {}

// pub mod ir {
//     pub mod types {
//         #[derive(Copy, Clone, PartialEq, Eq, Hash)]
//         pub struct Type(u16);
//         pub const I64: Type = Type(0x77);
//     }

//     #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
//     pub enum Endianness {
//         Little,
//         Big,
//     }

//     #[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
//     pub struct UserExternalNameRef(u32);
//     #[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
//     pub struct UserExternalName {
//         /// Arbitrary.
//         pub namespace: u32,
//         /// Arbitrary.
//         pub index: u32,
//     }

//     #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
//     pub struct TrapCode();


//     const BIT_LITTLE_ENDIAN: u16 = 1 << 2;
//     const BIT_BIG_ENDIAN: u16 = 1 << 3;
//     const BIT_ALIGNED: u16 = 1 << 0;

//     #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
//     pub struct MemFlags {
//         bits: u16,
//     }
//     impl MemFlags {
//         /// Create a new empty set of flags.
//         pub const fn new() -> Self {
//             Self { bits: 0 }.with_trap_code(Some(TrapCode::HEAP_OUT_OF_BOUNDS))
//         }
    
//         /// Create a set of flags representing an access from a "trusted" address, meaning it's
//         /// known to be aligned and non-trapping.
//         pub const fn trusted() -> Self {
//             Self::new().with_notrap().with_aligned()
//         }

//         const fn with_bit(mut self, bit: u16) -> Self {
//             self.bits |= bit;
//             self
//         }

//         pub const fn with_endianness(self, endianness: Endianness) -> Self {
//             let res = match endianness {
//                 Endianness::Little => self.with_bit(BIT_LITTLE_ENDIAN),
//                 Endianness::Big => self.with_bit(BIT_BIG_ENDIAN),
//             };
//             assert!(!(res.read_bit(BIT_LITTLE_ENDIAN) && res.read_bit(BIT_BIG_ENDIAN)));
//             res
//         }

//         const fn read_bit(self, bit: u16) -> bool {
//             self.bits & bit != 0
//         }

//         pub const fn with_aligned(self) -> Self {
//             self.with_bit(BIT_ALIGNED)
//         }
//     }

//     #[derive(Debug, Clone, PartialEq, Eq, Hash)]
//     pub enum ExternalName {
//         /// A reference to a name in a user-defined symbol table.
//         User(UserExternalNameRef),
//         /// A test case function name of up to a hardcoded amount of ascii
//         /// characters. This is not intended to be used outside test cases.
//         TestCase(),
//         /// A well-known runtime library function.
//         LibCall(LibCall),
//         /// A well-known symbol.
//         KnownSymbol(),
//     }

//     pub struct RelSourceLoc(u32);
//     pub struct SourceLoc(u32);

//     #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
//     pub enum LibCall {
//         /// probe for stack overflow. These are emitted for functions which need
//         /// when the `enable_probestack` setting is true.
//         Probestack,
//         /// ceil.f32
//         CeilF32,
//         /// ceil.f64
//         CeilF64,
//         /// floor.f32
//         FloorF32,
//         /// floor.f64
//         FloorF64,
//         /// trunc.f32
//         TruncF32,
//         /// frunc.f64
//         TruncF64,
//         /// nearest.f32
//         NearestF32,
//         /// nearest.f64
//         NearestF64,
//         /// fma.f32
//         FmaF32,
//         /// fma.f64
//         FmaF64,
//         /// libc.memcpy
//         Memcpy,
//         /// libc.memset
//         Memset,
//         /// libc.memmove
//         Memmove,
//         /// libc.memcmp
//         Memcmp,

//         /// Elf __tls_get_addr
//         ElfTlsGetAddr,
//         /// Elf __tls_get_offset
//         ElfTlsGetOffset,

//         /// The `pshufb` on x86 when SSSE3 isn't available.
//         X86Pshufb,
//         // When adding a new variant make sure to add it to `all_libcalls` too.
//     }
// }

// #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct Writable<T> {
//     reg: T,
// }

// #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct VReg {
//     bits: u32,
// }

// #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct Reg(VReg);

// pub mod isa {

//     #[derive(Clone)]
//     pub struct IsaBuilder<T> {
//         // triple: Triple,
//         // setup: settings::Builder,
//         // constructor: fn(Triple, settings::Flags, &settings::Builder) -> T,
//     }
//     #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
//     pub enum CallConv {
//         Fast,
//         Cold,
//         Tail,
//         SystemV,
//         WindowsFastcall,
//         AppleAarch64,
//         Probestack,
//         Winch,
//     }

//     pub mod unwind {
//         #[derive(Debug, Clone, Copy, PartialEq, Eq)]
//         #[non_exhaustive]
//         pub enum UnwindInfoKind {
//             /// No unwind info.
//             None,
//             /// SystemV CIE/FDE unwind info.
//             SystemV,
//             /// Windows X64 Unwind info
//             Windows,
//         }
        
//         /// Represents unwind information for a single function.
//         #[derive(Clone, Debug, PartialEq, Eq)]
//         #[non_exhaustive]
//         pub enum UnwindInfo {
//             /// Windows x64 ABI unwind information.
//             WindowsX64(),
//             /// System V ABI unwind information.
//             SystemV(),
//             /// Windows Arm64 ABI unwind information.
//             WindowsArm64(),
//         }
//     }

//     pub mod aarch64 {
//         pub mod inst {
//             pub mod emit {
//                 pub struct EmitInfo(crate::cranelift_codegen::settings::Flags);

//                 #[derive(Default, Clone, Debug)]
//                 pub struct EmitState {}
//             }

//             #[derive(Clone, Copy, Debug, PartialEq, Eq)]
//             pub enum OperandSize {
//                 /// 32-bit.
//                 Size32,
//                 /// 64-bit.
//                 Size64,
//             }

//             #[derive(Clone, Copy, Debug)]
//             pub struct NZCV {
//                 /// The negative condition flag.
//                 n: bool,
//                 /// The zero condition flag.
//                 z: bool,
//                 /// The carry condition flag.
//                 c: bool,
//                 /// The overflow condition flag.
//                 v: bool,
//             }

//             #[derive(Clone, Copy, Debug)]
//             pub struct UImm5 {
//                 value: u8,
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum IntToFpuOp {
//                 U32ToF32,
//                 I32ToF32,
//                 U32ToF64,
//                 I32ToF64,
//                 U64ToF32,
//                 I64ToF32,
//                 U64ToF64,
//                 I64ToF64,
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum FpuToIntOp {
//                 F32ToU32,
//                 F32ToI32,
//                 F32ToU64,
//                 F32ToI64,
//                 F64ToU32,
//                 F64ToI32,
//                 F64ToU64,
//                 F64ToI64,
//             }

//             #[derive(Clone, Copy, Debug, PartialEq, Eq)]
//             pub enum ScalarSize {
//                 Size8,
//                 Size16,
//                 Size32,
//                 Size64,
//                 Size128,
//             }

//             #[derive(Clone, Copy, Debug, PartialEq, Eq)]
//             pub enum VectorSize {
//                 Size8x8,
//                 Size8x16,
//                 Size16x4,
//                 Size16x8,
//                 Size32x2,
//                 Size32x4,
//                 Size64x2,
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum VecLanesOp {
//                 Addv,
//                 Uminv,
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum VecMisc2 {
//                 Not,
//                 Neg,
//                 Abs
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum ALUOp {
//                 Add,
//                 Sub,
//                 Orr,
//                 OrrNot,
//                 And,
//                 AndS,
//                 AndNot,
//                 Eor,
//                 EorNot,
//                 AddS,
//                 SubS,
//                 SMulH,
//                 UMulH,
//                 SDiv,
//                 UDiv,
//                 RotR,
//                 Lsr,
//                 Asr,
//                 Lsl,
//                 Adc,
//                 AdcS,
//                 Sbc,
//                 SbcS,
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum ALUOp3 {
//                 MAdd,
//                 MSub,
//                 UMAddL,
//                 SMAddL,
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum BitOp {
//                 RBit,
//                 Clz,
//                 Cls,
//                 Rev16,
//                 Rev32,
//                 Rev64,
//             }

//             #[derive(Clone, Copy, Debug, PartialEq, Eq)]
//             pub enum BranchTarget {
//                 /// An unresolved reference to a Label, as passed into
//                 /// `lower_branch_group()`.
//                 Label(crate::cranelift_codegen::MachLabel),
//                 /// A fixed PC offset.
//                 ResolvedOffset(i32),
//             }

//             #[derive(Clone, Copy, Debug, PartialEq)]
//             pub struct ASIMDFPModImm {
//                 imm: u8,
//                 size: ScalarSize,
//             }
//             /// Condition for conditional branches.
//             #[derive(Clone, Copy, Debug, PartialEq, Eq)]
//             #[repr(u8)]
//             pub enum Cond {
//                 /// Equal.
//                 Eq = 0,
//                 /// Not equal.
//                 Ne = 1,
//                 /// Unsigned greater than or equal to.
//                 Hs = 2,
//                 /// Unsigned less than.
//                 Lo = 3,
//                 /// Minus, negative.
//                 Mi = 4,
//                 /// Positive or zero.
//                 Pl = 5,
//                 /// Signed overflow.
//                 Vs = 6,
//                 /// No signed overflow.
//                 Vc = 7,
//                 /// Unsigned greater than.
//                 Hi = 8,
//                 /// Unsigned less than or equal to.
//                 Ls = 9,
//                 /// Signed greater or equal to.
//                 Ge = 10,
//                 /// Signed less than.
//                 Lt = 11,
//                 /// Signed greater than.
//                 Gt = 12,
//                 /// Signed less than or equal.
//                 Le = 13,
//                 /// Always executed.
//                 Al = 14,
//                 /// Always executed.
//                 Nv = 15,
//             }

//             #[derive(Clone, Debug)]
//             pub enum CondBrKind {
//                 /// Condition: given register is zero.
//                 Zero(crate::reg::Reg, crate::masm::OperandSize),
//                 /// Condition: given register is nonzero.
//                 NotZero(crate::reg::Reg, crate::masm::OperandSize),
//                 /// Condition: the given condition-code test is true.
//                 Cond(Cond),
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum FPUOp1 {
//                 Abs,
//                 Neg,
//                 Sqrt,
//                 Cvt32To64,
//                 Cvt64To32,
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum FPUOp2 {
//                 Add,
//                 Sub,
//                 Mul,
//                 Div,
//                 Max,
//                 Min,
//             }

//             #[derive(Copy, Clone, Debug)]
//             pub enum FPUOpRI {
//                 /// Unsigned right shift. Rd = Rn << #imm
//                 UShr32(FPURightShiftImm),
//                 /// Unsigned right shift. Rd = Rn << #imm
//                 UShr64(FPURightShiftImm),
//             }

//             #[derive(Clone, Copy, Debug)]
//             pub struct FPULeftShiftImm {
//                 /// Shift amount.
//                 pub amount: u8,
//                 /// Lane size in bits.
//                 pub lane_size_in_bits: u8,
//             }

//             #[derive(Clone, Copy, Debug)]
//             pub struct FPURightShiftImm {
//                 /// Shift amount.
//                 pub amount: u8,
//                 /// Lane size in bits.
//                 pub lane_size_in_bits: u8,
//             }

//             #[derive(Copy, Clone, Debug)]
//             pub enum FPUOpRIMod {
//                 /// Shift left and insert. Rd |= Rn << #imm
//                 Sli32(FPULeftShiftImm),
//                 /// Shift left and insert. Rd |= Rn << #imm
//                 Sli64(FPULeftShiftImm),
//             }

//             #[derive(Copy, Clone, PartialEq, Eq, Debug)]
//             pub enum FpuRoundMode {
//                 Minus32,
//                 Minus64,
//                 Plus32,
//                 Plus64,
//                 Zero32,
//                 Zero64,
//                 Nearest32,
//                 Nearest64,
//             }

//             #[derive(Copy, Clone, Debug, PartialEq)]
//             pub struct ImmLogic {
//                 /// The actual value.
//                 value: u64,
//                 /// `N` flag.
//                 pub n: bool,
//                 /// `S` field: element size and element bits.
//                 pub r: u8,
//                 /// `R` field: rotate amount.
//                 pub s: u8,
//                 /// Was this constructed for a 32-bit or 64-bit instruction?
//                 pub size: crate::masm::OperandSize,
//             }

//             #[derive(Copy, Clone, Debug)]
//             pub struct ImmShift {
//                 /// 6-bit shift amount.
//                 pub imm: u8,
//             }

//             #[derive(Copy, Clone, Debug)]
//             pub struct Imm12 {
//                 /// The immediate bits.
//                 pub bits: u16,
//                 /// Whether the immediate bits are shifted left by 12 or not.
//                 pub shift12: bool,
//             }

//             #[derive(Clone, Copy, Debug)]
//             #[repr(u8)]
//             pub enum ExtendOp {
//                 /// Unsigned extend byte.
//                 UXTB = 0b000,
//                 /// Unsigned extend halfword.
//                 UXTH = 0b001,
//                 /// Unsigned extend word.
//                 UXTW = 0b010,
//                 /// Unsigned extend doubleword.
//                 UXTX = 0b011,
//                 /// Signed extend byte.
//                 SXTB = 0b100,
//                 /// Signed extend halfword.
//                 SXTH = 0b101,
//                 /// Signed extend word.
//                 SXTW = 0b110,
//                 /// Signed extend doubleword.
//                 SXTX = 0b111,
//             }

//             #[derive(Clone, Debug)]
//             pub enum AMode {}

//             #[derive(Clone, Debug)]
//             pub enum PairAMode {}

//             #[derive(Clone, Copy, Debug)]
//             pub struct SImm7Scaled {
//                 /// The value.
//                 pub value: i16,
//                 /// multiplied by the size of this type
//                 pub scale_ty: crate::cranelift_codegen::ir::types::Type,
//             }

//             #[derive(Clone, Copy, Debug)]
//             pub struct SImm9 {
//                 /// The value.
//                 pub value: i16,
//             }

//             #[derive(Clone, Debug)]
//             pub enum MInst {
//                 Nop0,
//                 Nop4,
//                 AluRRR {
//                     alu_op: ALUOp,
//                     size: crate::masm::OperandSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                 },
//                 AluRRRR {
//                     alu_op: ALUOp3,
//                     size: crate::masm::OperandSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     ra: crate::reg::Reg,
//                 },
//                 AluRRImm12 {
//                     alu_op: ALUOp,
//                     size: crate::masm::OperandSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     imm12: Imm12,
//                 },
//                 AluRRImmLogic {
//                     alu_op: ALUOp,
//                     size: crate::masm::OperandSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     imml: ImmLogic,
//                 },
//                 AluRRImmShift {
//                     alu_op: ALUOp,
//                     size: crate::masm::OperandSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     immshift: ImmShift,
//                 },
//                 AluRRRShift {
//                     alu_op: ALUOp,
//                     size: crate::masm::OperandSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     shiftop: ShiftOpAndAmt,
//                 },
//                 AluRRRExtend {
//                     alu_op: ALUOp,
//                     size: crate::masm::OperandSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     extendop: ExtendOp,
//                 },
//                 BitRR {
//                     op: BitOp,
//                     size: crate::masm::OperandSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 ULoad8 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 SLoad8 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 ULoad16 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 SLoad16 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 ULoad32 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 SLoad32 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 ULoad64 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 Store8 {
//                     rd: crate::reg::Reg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 Store16 {
//                     rd: crate::reg::Reg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 Store32 {
//                     rd: crate::reg::Reg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 Store64 {
//                     rd: crate::reg::Reg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 StoreP64 {
//                     rt: crate::reg::Reg,
//                     rt2: crate::reg::Reg,
//                     mem: PairAMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 LoadP64 {
//                     rt: crate::reg::WritableReg,
//                     rt2: crate::reg::WritableReg,
//                     mem: PairAMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 Mov {
//                     size: crate::masm::OperandSize,
//                     rd: crate::reg::WritableReg,
//                     rm: crate::reg::Reg,
//                 },
//                 MovFromPReg {
//                     rd: crate::reg::WritableReg,
//                     rm: crate::regalloc2::PReg,
//                 },
//                 MovToPReg {
//                     rd: crate::regalloc2::PReg,
//                     rm: crate::reg::Reg,
//                 },
//                 MovWide {
//                     op: MoveWideOp,
//                     rd: crate::reg::WritableReg,
//                     imm: MoveWideConst,
//                     size: crate::masm::OperandSize,
//                 },
//                 MovK {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     imm: MoveWideConst,
//                     size: crate::masm::OperandSize,
//                 },
//                 Extend {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     signed: bool,
//                     from_bits: u8,
//                     to_bits: u8,
//                 },
//                 CSel {
//                     rd: crate::reg::WritableReg,
//                     cond: Cond,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                 },
//                 CSNeg {
//                     rd: crate::reg::WritableReg,
//                     cond: Cond,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                 },
//                 CSet {
//                     rd: crate::reg::WritableReg,
//                     cond: Cond,
//                 },
//                 CSetm {
//                     rd: crate::reg::WritableReg,
//                     cond: Cond,
//                 },
//                 CCmp {
//                     size: crate::masm::OperandSize,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     nzcv: NZCV,
//                     cond: Cond,
//                 },
//                 CCmpImm {
//                     size: crate::masm::OperandSize,
//                     rn: crate::reg::Reg,
//                     imm: UImm5,
//                     nzcv: NZCV,
//                     cond: Cond,
//                 },
//                 AtomicRMWLoop {
//                     ty: crate::cranelift_codegen::ir::types::Type,
//                     op: AtomicRMWLoopOp,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                     addr: crate::reg::Reg,
//                     operand: crate::reg::Reg,
//                     oldval: crate::reg::WritableReg,
//                     scratch1: crate::reg::WritableReg,
//                     scratch2: crate::reg::WritableReg,
//                 },
//                 AtomicCASLoop {
//                     ty: crate::cranelift_codegen::ir::types::Type,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                     addr: crate::reg::Reg,
//                     expected: crate::reg::Reg,
//                     replacement: crate::reg::Reg,
//                     oldval: crate::reg::WritableReg,
//                     scratch: crate::reg::WritableReg,
//                 },
//                 AtomicRMW {
//                     op: AtomicRMWOp,
//                     rs: crate::reg::Reg,
//                     rt: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     ty: crate::cranelift_codegen::ir::types::Type,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 AtomicCAS {
//                     rd: crate::reg::WritableReg,
//                     rs: crate::reg::Reg,
//                     rt: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     ty: crate::cranelift_codegen::ir::types::Type,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 LoadAcquire {
//                     access_ty: crate::cranelift_codegen::ir::types::Type,
//                     rt: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 StoreRelease {
//                     access_ty: crate::cranelift_codegen::ir::types::Type,
//                     rt: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 Fence,
//                 Csdb,
//                 FpuMove32 {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 FpuMove64 {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 FpuMove128 {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 FpuMoveFromVec {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     idx: u8,
//                     size: VectorSize,
//                 },
//                 FpuExtend {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     size: ScalarSize,
//                 },
//                 FpuRR {
//                     fpu_op: FPUOp1,
//                     size: ScalarSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 FpuRRR {
//                     fpu_op: FPUOp2,
//                     size: ScalarSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                 },
//                 FpuRRI {
//                     fpu_op: FPUOpRI,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 FpuRRIMod {
//                     fpu_op: FPUOpRIMod,
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                 },
//                 FpuRRRR {
//                     fpu_op: FPUOp3,
//                     size: ScalarSize,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     ra: crate::reg::Reg,
//                 },
//                 FpuCmp {
//                     size: ScalarSize,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                 },
//                 FpuLoad16 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuStore16 {
//                     rd: crate::reg::Reg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuLoad32 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuStore32 {
//                     rd: crate::reg::Reg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuLoad64 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuStore64 {
//                     rd: crate::reg::Reg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuLoad128 {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuStore128 {
//                     rd: crate::reg::Reg,
//                     mem: AMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuLoadP64 {
//                     rt: crate::reg::WritableReg,
//                     rt2: crate::reg::WritableReg,
//                     mem: PairAMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuStoreP64 {
//                     rt: crate::reg::Reg,
//                     rt2: crate::reg::Reg,
//                     mem: PairAMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuLoadP128 {
//                     rt: crate::reg::WritableReg,
//                     rt2: crate::reg::WritableReg,
//                     mem: PairAMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuStoreP128 {
//                     rt: crate::reg::Reg,
//                     rt2: crate::reg::Reg,
//                     mem: PairAMode,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 FpuToInt {
//                     op: FpuToIntOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 IntToFpu {
//                     op: IntToFpuOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 FpuCSel16 {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     cond: Cond,
//                 },
//                 FpuCSel32 {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     cond: Cond,
//                 },
//                 FpuCSel64 {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     cond: Cond,
//                 },
//                 FpuRound {
//                     op: FpuRoundMode,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 MovToFpu {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     size: ScalarSize,
//                 },
//                 FpuMoveFPImm {
//                     rd: crate::reg::WritableReg,
//                     imm: ASIMDFPModImm,
//                     size: ScalarSize,
//                 },
//                 MovToVec {
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     idx: u8,
//                     size: VectorSize,
//                 },
//                 MovFromVec {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     idx: u8,
//                     size: ScalarSize,
//                 },
//                 MovFromVecSigned {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     idx: u8,
//                     size: VectorSize,
//                     scalar_size: crate::masm::OperandSize,
//                 },
//                 VecDup {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     size: VectorSize,
//                 },
//                 VecDupFromFpu {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     size: VectorSize,
//                     lane: u8,
//                 },
//                 VecDupFPImm {
//                     rd: crate::reg::WritableReg,
//                     imm: ASIMDFPModImm,
//                     size: VectorSize,
//                 },
//                 VecDupImm {
//                     rd: crate::reg::WritableReg,
//                     imm: ASIMDMovModImm,
//                     invert: bool,
//                     size: VectorSize,
//                 },
//                 VecExtend {
//                     t: VecExtendOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     high_half: bool,
//                     lane_size: ScalarSize,
//                 },
//                 VecMovElement {
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     dest_idx: u8,
//                     src_idx: u8,
//                     size: VectorSize,
//                 },
//                 VecRRLong {
//                     op: VecRRLongOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     high_half: bool,
//                 },
//                 VecRRNarrowLow {
//                     op: VecRRNarrowOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     lane_size: ScalarSize,
//                 },
//                 VecRRNarrowHigh {
//                     op: VecRRNarrowOp,
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     lane_size: ScalarSize,
//                 },
//                 VecRRPair {
//                     op: VecPairOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 VecRRRLong {
//                     alu_op: VecRRRLongOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     high_half: bool,
//                 },
//                 VecRRRLongMod {
//                     alu_op: VecRRRLongModOp,
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     high_half: bool,
//                 },
//                 VecRRPairLong {
//                     op: VecRRPairLongOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                 },
//                 VecRRR {
//                     alu_op: VecALUOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     size: VectorSize,
//                 },
//                 VecRRRMod {
//                     alu_op: VecALUModOp,
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     size: VectorSize,
//                 },
//                 VecFmlaElem {
//                     alu_op: VecALUModOp,
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     size: VectorSize,
//                     idx: u8,
//                 },
//                 VecMisc {
//                     op: VecMisc2,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     size: VectorSize,
//                 },
//                 VecLanes {
//                     op: VecLanesOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     size: VectorSize,
//                 },
//                 VecShiftImm {
//                     op: VecShiftImmOp,
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     size: VectorSize,
//                     imm: u8,
//                 },
//                 VecShiftImmMod {
//                     op: VecShiftImmModOp,
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     size: VectorSize,
//                     imm: u8,
//                 },
//                 VecExtract {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     imm4: u8,
//                 },
//                 VecTbl {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                 },
//                 VecTblExt {
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                 },
//                 VecTbl2 {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rn2: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                 },
//                 VecTbl2Ext {
//                     rd: crate::reg::WritableReg,
//                     ri: crate::reg::Reg,
//                     rn: crate::reg::Reg,
//                     rn2: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                 },
//                 VecLoadReplicate {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     size: VectorSize,
//                     flags: crate::cranelift_codegen::ir::MemFlags,
//                 },
//                 VecCSel {
//                     rd: crate::reg::WritableReg,
//                     rn: crate::reg::Reg,
//                     rm: crate::reg::Reg,
//                     cond: Cond,
//                 },
//                 MovToNZCV {
//                     rn: crate::reg::Reg,
//                 },
//                 MovFromNZCV {
//                     rd: crate::reg::WritableReg,
//                 },
//                 Call {
//                     info: BoxCallInfo,
//                 },
//                 CallInd {
//                     info: BoxCallIndInfo,
//                 },
//                 ReturnCall {
//                     info: BoxReturnCallInfo,
//                 },
//                 ReturnCallInd {
//                     info: BoxReturnCallIndInfo,
//                 },
//                 Args {
//                     args: VecArgPair,
//                 },
//                 Rets {
//                     rets: VecRetPair,
//                 },
//                 Ret,
//                 AuthenticatedRet {
//                     key: APIKey,
//                     is_hint: bool,
//                 },
//                 Jump {
//                     dest: BranchTarget,
//                 },
//                 CondBr {
//                     taken: BranchTarget,
//                     not_taken: BranchTarget,
//                     kind: CondBrKind,
//                 },
//                 TestBitAndBranch {
//                     kind: TestBitAndBranchKind,
//                     taken: BranchTarget,
//                     not_taken: BranchTarget,
//                     rn: crate::reg::Reg,
//                     bit: u8,
//                 },
//                 TrapIf {
//                     kind: CondBrKind,
//                     trap_code: TrapCode,
//                 },
//                 IndirectBr {
//                     rn: crate::reg::Reg,
//                     targets: VecMachLabel,
//                 },
//                 Brk,
//                 Udf {
//                     trap_code: TrapCode,
//                 },
//                 Adr {
//                     rd: crate::reg::WritableReg,
//                     off: i32,
//                 },
//                 Adrp {
//                     rd: crate::reg::WritableReg,
//                     off: i32,
//                 },
//                 Word4 {
//                     data: u32,
//                 },
//                 Word8 {
//                     data: u64,
//                 },
//                 JTSequence {
//                     default: crate::cranelift_codegen::MachLabel,
//                     targets: BoxVecMachLabel,
//                     ridx: crate::reg::Reg,
//                     rtmp1: crate::reg::WritableReg,
//                     rtmp2: crate::reg::WritableReg,
//                 },
//                 LoadExtName {
//                     rd: crate::reg::WritableReg,
//                     name: BoxExternalName,
//                     offset: i64,
//                 },
//                 LoadAddr {
//                     rd: crate::reg::WritableReg,
//                     mem: AMode,
//                 },
//                 Paci {
//                     key: APIKey,
//                 },
//                 Xpaclri,
//                 Bti {
//                     targets: BranchTarget,
//                 },
//                 EmitIsland {
//                     needed_space: CodeOffset,
//                 },
//                 ElfTlsGetAddr {
//                     symbol: BoxExternalName,
//                     rd: crate::reg::WritableReg,
//                     tmp: crate::reg::WritableReg,
//                 },
//                 MachOTlsGetAddr {
//                     symbol: ExternalName,
//                     rd: crate::reg::WritableReg,
//                 },
//                 Unwind {
//                     inst: UnwindInst,
//                 },
//                 DummyUse {
//                     reg: crate::reg::Reg,
//                 },
//                 StackProbeLoop {
//                     start: crate::reg::WritableReg,
//                     end: crate::reg::Reg,
//                     step: Imm12,
//                 },
//             }
//         }
//     }
// }
