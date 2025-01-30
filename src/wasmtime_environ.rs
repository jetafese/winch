
pub const FUNCREF_INIT_BIT: usize = 1;
pub const FUNCREF_MASK: usize = !FUNCREF_INIT_BIT;

pub struct WasmparserTypeConverter<'a, F> {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum EngineOrModuleTypeIndex {
    Engine(_),
    Module(_),
    RecGroup(_),
}

pub trait TypeConvert {
    /// Converts a wasmparser heap type to a wasmtime type
    fn convert_heap_type(&self, ty: crate::wasmparser::HeapType) -> WasmHeapType {
        match ty {
            crate::wasmparser::HeapType::Concrete(i) => self.lookup_heap_type(i),
            crate::wasmparser::HeapType::Abstract { ty, shared: false } => match ty {
                crate::wasmparser::AbstractHeapType::Extern => WasmHeapType::Extern,
                crate::wasmparser::AbstractHeapType::NoExtern => WasmHeapType::NoExtern,
                crate::wasmparser::AbstractHeapType::Func => WasmHeapType::Func,
                crate::wasmparser::AbstractHeapType::NoFunc => WasmHeapType::NoFunc,
                crate::wasmparser::AbstractHeapType::Any => WasmHeapType::Any,
                crate::wasmparser::AbstractHeapType::Eq => WasmHeapType::Eq,
                crate::wasmparser::AbstractHeapType::I31 => WasmHeapType::I31,
                crate::wasmparser::AbstractHeapType::Array => WasmHeapType::Array,
                crate::wasmparser::AbstractHeapType::Struct => WasmHeapType::Struct,
                crate::wasmparser::AbstractHeapType::None => WasmHeapType::None,

                crate::wasmparser::AbstractHeapType::Exn
                | crate::wasmparser::AbstractHeapType::NoExn
                | crate::wasmparser::AbstractHeapType::Cont
                | crate::wasmparser::AbstractHeapType::NoCont => {
                    unimplemented!("unsupported heap type {ty:?}");
                }
            },
            _ => unimplemented!("unsupported heap type {ty:?}"),
        }
    }

    /// Converts the specified type index from a heap type into a canonicalized
    /// heap type.
    fn lookup_heap_type(&self, index: crate::wasmparser::UnpackedIndex) -> WasmHeapType;

  /// Converts the specified type index from a heap type into a canonicalized
    /// heap type.
    fn lookup_type_index(&self, index: crate::wasmparser::UnpackedIndex) -> EngineOrModuleTypeIndex;
}

pub enum IndexType {
    I32,
    I64,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PrimaryMap<K, V> {}

#[derive(Default)]
pub struct ModuleTranslation<'data> {}
pub struct ModuleTypesBuilder {}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Limits {
    pub min: u64,
    pub max: Option<u64>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Memory {
    pub idx_type: IndexType,
    pub limits: Limits,
    pub shared: bool,
    pub page_size_log2: u8,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub struct Table {
    /// The type of the index used to access the table.
    pub idx_type: IndexType,
    /// Tables are constrained by limits for their minimum and optionally maximum size.
    /// The limits are given in numbers of entries.
    pub limits: Limits,
    /// The table elements' Wasm type.
    pub ref_type: WasmRefType,
}

pub trait Signed {
    /// The signed integer for this type which has the same width.
    type Signed;

    /// View this unsigned integer as a signed integer of the same width.
    ///
    /// All bits are preserved.
    fn signed(self) -> Self::Signed;
}

/// Index type of a function (imported or defined) inside the WebAssembly module.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct FuncIndex(u32);

/// Index type of a global variable (imported or defined) inside the WebAssembly module.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct GlobalIndex(u32);

/// Index type of a linear memory (imported or defined) inside the WebAssembly module.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct MemoryIndex(u32);

/// Index type of a table (imported or defined) inside the WebAssembly module.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct TableIndex(u32);

/// Index type of a type (imported or defined) inside the WebAssembly module.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct TypeIndex(u32);

pub struct WasmFuncType {
}

/// WebAssembly reference type -- equivalent of `wasmparser`'s RefType
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct WasmRefType {
    /// Whether or not this reference is nullable.
    pub nullable: bool,
    /// The heap type that this reference contains.
    pub heap_type: WasmHeapType,
}

/// WebAssembly value type -- equivalent of `wasmparser::ValType`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum WasmValType {
    /* copied from crates/environ/src/types */
    /// I32 type
    I32,
    /// I64 type
    I64,
    /// F32 type
    F32,
    /// F64 type
    F64,
    /// V128 type
    V128,
    /// Reference type
    Ref(WasmRefType),
}

/// WebAssembly heap type -- equivalent of `wasmparser`'s HeapType
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
// #[allow(missing_docs, reason = "self-describing variants")]
pub enum WasmHeapType {
    // External types.
    Extern,
    NoExtern,

    // Function types.
    Func,
    NoFunc,

    // Internal types.
    Any,
    Eq,
    I31,
    Array,
    Struct,
    None,
}

/// Helper macro to define a builtin type such as `BuiltinFunctionIndex` and
/// `ComponentBuiltinFunctionIndex` using the iterator macro, e.g.
/// `foreach_builtin_function`, as the way to generate accessor methods.
macro_rules! declare_builtin_index {
    ($index_name:ident, $iter:ident) => {
        /// An index type for builtin functions.
        #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $index_name(u32);

        impl $index_name {
            /// Create a new builtin from its raw index
            pub const fn from_u32(i: u32) -> Self {
                assert!(i < Self::len());
                Self(i)
            }

            /// Return the index as an u32 number.
            pub const fn index(&self) -> u32 {
                self.0
            }

            $iter!(declare_builtin_index_constructors);
        }
    };
}

/// Helper macro used by the above macro.
macro_rules! declare_builtin_index_constructors {
    (
        $(
            $( #[$attr:meta] )*
            $name:ident( $( $pname:ident: $param:ident ),* ) $( -> $result:ident )?;
        )*
    ) => {
        declare_builtin_index_constructors!(
            @indices;
            0;
            $( $( #[$attr] )* $name; )*
        );

        /// Returns a symbol name for this builtin.
        pub fn name(&self) -> &'static str {
            $(
                $( #[$attr] )*
                if *self == Self::$name() {
                    return stringify!($name);
                }
            )*
            unreachable!()
        }
    };

    // Base case: no more indices to declare, so define the total number of
    // function indices.
    (
        @indices;
        $len:expr;
    ) => {
        /// Returns the total number of builtin functions.
        pub const fn len() -> u32 {
            $len
        }
    };

    // Recursive case: declare the next index, and then keep declaring the rest of
    // the indices.
    (
         @indices;
         $index:expr;
         $( #[$this_attr:meta] )*
         $this_name:ident;
         $(
             $( #[$rest_attr:meta] )*
             $rest_name:ident;
         )*
    ) => {
        $( #[$this_attr] )*
        #[allow(missing_docs, reason = "macro-generated")]
        pub const fn $this_name() -> Self {
            Self($index)
        }

        declare_builtin_index_constructors!(
            @indices;
            ($index + 1);
            $( $( #[$rest_attr] )* $rest_name; )*
        );
    }
}

use crate::foreach_builtin_function;
// Define `struct BuiltinFunctionIndex`
declare_builtin_index!(BuiltinFunctionIndex, foreach_builtin_function);

macro_rules! define_tunables {
    (
        $(#[$outer_attr:meta])*
        pub struct $tunables:ident {
            $(
                $(#[$field_attr:meta])*
                pub $field:ident : $field_ty:ty,
            )*
        }

        pub struct $config_tunables:ident {
            ...
        }
    ) => {
        $(#[$outer_attr])*
        pub struct $tunables {
            $(
                $(#[$field_attr])*
                pub $field: $field_ty,
            )*
        }

        /// Optional tunable configuration options used in `wasmtime::Config`
        #[derive(Default, Clone)]
        #[allow(missing_docs, reason = "macro-generated fields")]
        pub struct $config_tunables {
            $(pub $field: Option<$field_ty>,)*
        }

        impl $config_tunables {
            /// Formats configured fields into `f`.
            // pub fn format(&self, f: &mut fmt::DebugStruct<'_,'_>) {
            //     $(
            //         if let Some(val) = &self.$field {
            //             f.field(stringify!($field), val);
            //         }
            //     )*
            // }

            /// Configure the `Tunables` provided.
            pub fn configure(&self, tunables: &mut Tunables) {
                $(
                    if let Some(val) = self.$field {
                        tunables.$field = val;
                    }
                )*
            }
        }
    };
}

/// The garbage collector implementation to use.
#[derive(Clone, Copy, Hash, Debug, PartialEq, Eq)]
pub enum Collector {
    /// The deferred reference-counting collector.
    DeferredReferenceCounting,
    /// The null collector.
    Null,
}

define_tunables! {
    /// Tunable parameters for WebAssembly compilation.
    #[derive(Clone, Hash, Debug)]
    pub struct Tunables {
        /// The garbage collector implementation to use, which implies the layout of
        /// GC objects and barriers that must be emitted in Wasm code.
        pub collector: Option<Collector>,

        /// Initial size, in bytes, to be allocated for linear memories.
        pub memory_reservation: u64,

        /// The size, in bytes, of the guard page region for linear memories.
        pub memory_guard_size: u64,

        /// The size, in bytes, to allocate at the end of a relocated linear
        /// memory for growth.
        pub memory_reservation_for_growth: u64,

        /// Whether or not to generate native DWARF debug information.
        pub generate_native_debuginfo: bool,

        /// Whether or not to retain DWARF sections in compiled modules.
        pub parse_wasm_debuginfo: bool,

        /// Whether or not fuel is enabled for generated code, meaning that fuel
        /// will be consumed every time a wasm instruction is executed.
        pub consume_fuel: bool,

        /// Whether or not we use epoch-based interruption.
        pub epoch_interruption: bool,

        /// Whether or not linear memories are allowed to be reallocated after
        /// initial allocation at runtime.
        pub memory_may_move: bool,

        /// Whether or not linear memory allocations will have a guard region at the
        /// beginning of the allocation in addition to the end.
        pub guard_before_linear_memory: bool,

        /// Whether to initialize tables lazily, so that instantiation is fast but
        /// indirect calls are a little slower. If false, tables are initialized
        /// eagerly from any active element segments that apply to them during
        /// instantiation.
        pub table_lazy_init: bool,

        /// Indicates whether an address map from compiled native code back to wasm
        /// offsets in the original file is generated.
        pub generate_address_map: bool,

        /// Flag for the component module whether adapter modules have debug
        /// assertions baked into them.
        pub debug_adapter_modules: bool,

        /// Whether or not lowerings for relaxed simd instructions are forced to
        /// be deterministic.
        pub relaxed_simd_deterministic: bool,

        /// Whether or not Wasm functions target the winch abi.
        pub winch_callable: bool,

        /// Whether or not the host will be using native signals (e.g. SIGILL,
        /// SIGSEGV, etc) to implement traps.
        pub signals_based_traps: bool,

        /// Whether CoW images might be used to initialize linear memories.
        pub memory_init_cow: bool,
    }

    pub struct ConfigTunables {
        ...
    }
}

/// This class computes offsets to fields within `VMContext` and other
/// related structs that JIT code accesses directly.
#[derive(Debug, Clone, Copy)]
pub struct VMOffsets<P> {
    /// The size in bytes of a pointer on the target.
    pub ptr: P,
    /// The number of imported functions in the module.
    pub num_imported_functions: u32,
    /// The number of imported tables in the module.
    pub num_imported_tables: u32,
    /// The number of imported memories in the module.
    pub num_imported_memories: u32,
    /// The number of imported globals in the module.
    pub num_imported_globals: u32,
    /// The number of defined tables in the module.
    pub num_defined_tables: u32,
    /// The number of defined memories in the module.
    pub num_defined_memories: u32,
    /// The number of memories owned by the module instance.
    pub num_owned_memories: u32,
    /// The number of defined globals in the module.
    pub num_defined_globals: u32,
    /// The number of escaped functions in the module, the size of the func_refs
    /// array.
    pub num_escaped_funcs: u32,

    // precalculated offsets of various member fields
    imported_functions: u32,
    imported_tables: u32,
    imported_memories: u32,
    imported_globals: u32,
    defined_tables: u32,
    defined_memories: u32,
    owned_memories: u32,
    defined_globals: u32,
    defined_func_refs: u32,
    size: u32,
}

/// Align an offset used in this module to a specific byte-width by rounding up
#[inline]
fn align(offset: u32, width: u32) -> u32 {
    (offset + (width - 1)) / width * width
}

/// Trait used for the `ptr` representation of the field of `VMOffsets`
pub trait PtrSize {
    /// Returns the pointer size, in bytes, for the target.
    fn size(&self) -> u8;

    /// The offset of the `VMContext::runtime_limits` field
    fn vmcontext_runtime_limits(&self) -> u8 {
        u8::try_from(align(
            u32::try_from(core::mem::size_of::<u32>()).unwrap(),
            u32::from(self.size()),
        ))
        .unwrap()
    }

    /// The offset of the `VMContext::builtin_functions` field
    fn vmcontext_builtin_functions(&self) -> u8 {
        self.vmcontext_runtime_limits() + self.size()
    }

    /// The offset of the `array_call` field.
    #[inline]
    fn vm_func_ref_array_call(&self) -> u8 {
        0 * self.size()
    }

    /// The offset of the `wasm_call` field.
    #[inline]
    fn vm_func_ref_wasm_call(&self) -> u8 {
        1 * self.size()
    }

    /// The offset of the `type_index` field.
    #[inline]
    fn vm_func_ref_type_index(&self) -> u8 {
        2 * self.size()
    }

    /// The offset of the `vmctx` field.
    #[inline]
    fn vm_func_ref_vmctx(&self) -> u8 {
        3 * self.size()
    }

    /// Return the size of `VMFuncRef`.
    #[inline]
    fn size_of_vm_func_ref(&self) -> u8 {
        4 * self.size()
    }

    /// Return the size of `VMGlobalDefinition`; this is the size of the largest value type (i.e. a
    /// V128).
    #[inline]
    fn size_of_vmglobal_definition(&self) -> u8 {
        16
    }

    // Offsets within `VMRuntimeLimits`

    /// Return the offset of the `fuel_consumed` field of `VMRuntimeLimits`
    #[inline]
    fn vmruntime_limits_fuel_consumed(&self) -> u8 {
        0
    }

    /// Return the offset of the `epoch_deadline` field of `VMRuntimeLimits`
    #[inline]
    fn vmruntime_limits_epoch_deadline(&self) -> u8 {
        self.vmruntime_limits_fuel_consumed() + 8
    }

    /// Return the offset of the `stack_limit` field of `VMRuntimeLimits`
    #[inline]
    fn vmruntime_limits_stack_limit(&self) -> u8 {
        self.vmruntime_limits_epoch_deadline() + 8
    }

    /// Return the offset of the `last_wasm_exit_fp` field of `VMRuntimeLimits`.
    fn vmruntime_limits_last_wasm_exit_fp(&self) -> u8 {
        self.vmruntime_limits_stack_limit() + self.size()
    }

    /// Return the offset of the `last_wasm_exit_pc` field of `VMRuntimeLimits`.
    fn vmruntime_limits_last_wasm_exit_pc(&self) -> u8 {
        self.vmruntime_limits_last_wasm_exit_fp() + self.size()
    }

    /// Return the offset of the `last_wasm_entry_fp` field of `VMRuntimeLimits`.
    fn vmruntime_limits_last_wasm_entry_fp(&self) -> u8 {
        self.vmruntime_limits_last_wasm_exit_pc() + self.size()
    }

    // Offsets within `VMMemoryDefinition`

    /// The offset of the `base` field.
    #[inline]
    fn vmmemory_definition_base(&self) -> u8 {
        0 * self.size()
    }

    /// The offset of the `current_length` field.
    #[inline]
    fn vmmemory_definition_current_length(&self) -> u8 {
        1 * self.size()
    }

    /// Return the size of `VMMemoryDefinition`.
    #[inline]
    fn size_of_vmmemory_definition(&self) -> u8 {
        2 * self.size()
    }

    /// Return the size of `*mut VMMemoryDefinition`.
    #[inline]
    fn size_of_vmmemory_pointer(&self) -> u8 {
        self.size()
    }

    // Offsets within `VMArrayCallHostFuncContext`.

    /// Return the offset of `VMArrayCallHostFuncContext::func_ref`.
    fn vmarray_call_host_func_context_func_ref(&self) -> u8 {
        u8::try_from(align(
            u32::try_from(core::mem::size_of::<u32>()).unwrap(),
            u32::from(self.size()),
        ))
        .unwrap()
    }

    /// Return the offset to the `magic` value in this `VMContext`.
    #[inline]
    fn vmctx_magic(&self) -> u8 {
        // This is required by the implementation of `VMContext::instance` and
        // `VMContext::instance_mut`. If this value changes then those locations
        // need to be updated.
        0
    }

    /// Return the offset to the `VMRuntimeLimits` structure
    #[inline]
    fn vmctx_runtime_limits(&self) -> u8 {
        self.vmctx_magic() + self.size()
    }

    /// Return the offset to the `VMBuiltinFunctionsArray` structure
    #[inline]
    fn vmctx_builtin_functions(&self) -> u8 {
        self.vmctx_runtime_limits() + self.size()
    }

    /// Return the offset to the `callee` member in this `VMContext`.
    #[inline]
    fn vmctx_callee(&self) -> u8 {
        self.vmctx_builtin_functions() + self.size()
    }

    /// Return the offset to the `*const AtomicU64` epoch-counter
    /// pointer.
    #[inline]
    fn vmctx_epoch_ptr(&self) -> u8 {
        self.vmctx_callee() + self.size()
    }

    /// Return the offset to the GC heap base in this `VMContext`.
    #[inline]
    fn vmctx_gc_heap_base(&self) -> u8 {
        self.vmctx_epoch_ptr() + self.size()
    }

    /// Return the offset to the GC heap bound in this `VMContext`.
    #[inline]
    fn vmctx_gc_heap_bound(&self) -> u8 {
        self.vmctx_gc_heap_base() + self.size()
    }

    /// Return the offset to the `*mut T` collector-specific data.
    ///
    /// This is a pointer that different collectors can use however they see
    /// fit.
    #[inline]
    fn vmctx_gc_heap_data(&self) -> u8 {
        self.vmctx_gc_heap_bound() + self.size()
    }

    /// The offset of the `*const dyn Store` member.
    #[inline]
    fn vmctx_store(&self) -> u8 {
        self.vmctx_gc_heap_data() + self.size()
    }

    /// The offset of the `type_ids` array pointer.
    #[inline]
    fn vmctx_type_ids_array(&self) -> u8 {
        self.vmctx_store() + 2 * self.size()
    }

    /// The end of statically known offsets in `VMContext`.
    ///
    /// Data after this is dynamically sized.
    #[inline]
    fn vmctx_dynamic_data_start(&self) -> u8 {
        self.vmctx_type_ids_array() + self.size()
    }
}
