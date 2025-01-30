#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct TrapCode(_);

#[non_exhaustive]
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
// #[allow(missing_docs, reason = "self-describing variants")]
pub enum Trap {
    StackOverflow,
    MemoryOutOfBounds,
    HeapMisaligned,
    TableOutOfBounds,
    IndirectCallToNull,
    BadSignature,
    IntegerOverflow,
    IntegerDivisionByZero,
    BadConversionToInteger,
    UnreachableCodeReached,
    Interrupt,
    AlwaysTrapAdapter,
    OutOfFuel,
    AtomicWaitNonSharedMemory,
    NullReference,
    ArrayOutOfBounds,
    AllocationTooLarge,
    CastFailure,
    CannotEnterComponent,
}

const TRAP_OFFSET: u8 = 2;

pub const TRAP_BAD_SIGNATURE: TrapCode = TrapCode::unwrap_user(Trap::BadSignature as u8 + TRAP_OFFSET);

pub const TRAP_TABLE_OUT_OF_BOUNDS: TrapCode = TrapCode::unwrap_user(Trap::TableOutOfBounds as u8 + TRAP_OFFSET);

pub const TRAP_INDIRECT_CALL_TO_NULL: TrapCode =
    TrapCode::unwrap_user(Trap::IndirectCallToNull as u8 + TRAP_OFFSET);

pub struct CompiledFunction {}