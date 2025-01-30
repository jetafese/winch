//! Classification of code generation errors.


/// A code generation error.
#[derive(Debug)]
pub(crate) enum CodeGenError {
    /// 32-bit platform support.
    Unsupported32BitPlatform,
    /// Unsupported WebAssembly type.
    UnsupportedWasmType,
    /// Missing implementation for a current instruction.
    UnimplementedWasmInstruction,
    /// Unimplemented MacroAssembler instruction.
    UnimplementedMasmInstruction,
    /// Unsupported eager initialization of tables.
    UnsupportedTableEagerInit,
    /// An internal error.
    ///
    /// This error means that an internal invariant was not met and usually
    /// implies a compiler bug.
    Internal(InternalError),
}
impl std::fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl std::fmt::Display for InternalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl std::error::Error for CodeGenError {}
impl std::error::Error for InternalError {}

/// An internal error.
#[derive(Debug)]
pub(crate) enum InternalError {
    /// Register allocation error.
    ExpectedRegisterToBeAvailable,
    /// Control frame expected.
    ControlFrameExpected,
    /// Control frame for if expected.
    IfControlFrameExpected,
    /// Not enough values in the value stack.
    MissingValuesInStack,
    /// Unexpected operand size. 32 or 64 bits are supported.
    UnexpectedOperandSize,
    /// Accessing the value stack with an invalid index.
    UnexpectedValueStackIndex,
    /// Expects a specific state in the value stack.
    UnexpectedValueInValueStack,
    /// A mismatch occured in the control frame state.
    ControlFrameStateMismatch,
    /// Expected a specific table element value.
    TableElementValueExpected,
    /// Illegal fuel tracking state.
    IllegalFuelState,
    /// Missing special function argument.
    VMContextArgumentExpected,
    /// Expected memory location to be addressed via the stack pointer.
    SPAddressingExpected,
    /// Stack pointer offset is illegal.
    InvalidSPOffset,
    /// Unexpected function call at location.
    UnexpectedFunctionCall,
    /// Invalid local offset.
    InvalidLocalOffset,
    /// Unsupported immediate for instruction.
    UnsupportedImm,
    /// Invalid operand combination.
    InvalidOperandCombination,
    /// Invalid two argument form.
    InvalidTwoArgumentForm,
}

impl CodeGenError {
    pub(crate) const fn unsupported_wasm_type() -> Self {
        Self::UnsupportedWasmType
    }

    pub(crate) const fn unsupported_table_eager_init() -> Self {
        Self::UnsupportedTableEagerInit
    }

    pub(crate) const fn unimplemented_wasm_instruction() -> Self {
        Self::UnimplementedWasmInstruction
    }

    pub(crate) const fn unsupported_32_bit_platform() -> Self {
        Self::Unsupported32BitPlatform
    }

    pub(crate) const fn unexpected_function_call() -> Self {
        Self::Internal(InternalError::UnexpectedFunctionCall)
    }

    pub(crate) const fn sp_addressing_expected() -> Self {
        Self::Internal(InternalError::SPAddressingExpected)
    }

    pub(crate) const fn invalid_sp_offset() -> Self {
        Self::Internal(InternalError::InvalidSPOffset)
    }

    pub(crate) const fn expected_register_to_be_available() -> Self {
        Self::Internal(InternalError::ExpectedRegisterToBeAvailable)
    }

    pub(crate) fn vmcontext_arg_expected() -> Self {
        Self::Internal(InternalError::VMContextArgumentExpected)
    }

    pub(crate) const fn control_frame_expected() -> Self {
        Self::Internal(InternalError::ControlFrameExpected)
    }

    pub(crate) const fn if_control_frame_expected() -> Self {
        Self::Internal(InternalError::IfControlFrameExpected)
    }

    pub(crate) const fn missing_values_in_stack() -> Self {
        Self::Internal(InternalError::MissingValuesInStack)
    }

    pub(crate) const fn unexpected_operand_size() -> Self {
        Self::Internal(InternalError::UnexpectedOperandSize)
    }

    pub(crate) const fn unexpected_value_stack_index() -> Self {
        Self::Internal(InternalError::UnexpectedValueStackIndex)
    }

    pub(crate) const fn unexpected_value_in_value_stack() -> Self {
        Self::Internal(InternalError::UnexpectedValueInValueStack)
    }

    pub(crate) const fn control_frame_state_mismatch() -> Self {
        Self::Internal(InternalError::ControlFrameStateMismatch)
    }

    pub(crate) const fn table_element_value_expected() -> Self {
        Self::Internal(InternalError::TableElementValueExpected)
    }

    pub(crate) const fn illegal_fuel_state() -> Self {
        Self::Internal(InternalError::IllegalFuelState)
    }

    pub(crate) const fn invalid_local_offset() -> Self {
        Self::Internal(InternalError::InvalidLocalOffset)
    }

    pub(crate) const fn unsupported_imm() -> Self {
        Self::Internal(InternalError::UnsupportedImm)
    }

    pub(crate) const fn invalid_two_arg_form() -> Self {
        Self::Internal(InternalError::InvalidTwoArgumentForm)
    }

    pub(crate) const fn invalid_operand_combination() -> Self {
        Self::Internal(InternalError::InvalidOperandCombination)
    }

    pub(crate) const fn unimplemented_masm_instruction() -> Self {
        Self::UnimplementedMasmInstruction
    }
}
