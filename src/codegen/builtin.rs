//! Builtin function handling.

use crate::{
    abi::{ABISig, ABI},
    codegen::env::ptr_type_from_ptr_size,
    CallingConvention,
};
use crate::cranelift_codegen::ir::LibCall;
use std::sync::Arc;

use crate::wasmtime_environ::{BuiltinFunctionIndex, PtrSize, VMOffsets, WasmValType};


#[derive(Copy, Clone)]
pub(crate) enum BuiltinType {
    /// Dynamic built-in function, derived from the VMContext.
    Builtin(BuiltinFunctionIndex),
    /// A known libcall.
    /// See [`cranelift_codegen::ir::LibCall`] for more details.
    LibCall(LibCall),
}

impl BuiltinType {
    /// Creates a new builtin from a Wasmtime-defined builtin function
    /// enumerated with a [`BuiltinFunctionIndex`].
    pub fn builtin(idx: BuiltinFunctionIndex) -> Self {
        Self::Builtin(idx)
    }

    /// Creates a new builtin from a Compiler-defined [`LibCall`] typically used
    /// late in lowering.
    pub fn libcall(libcall: LibCall) -> Self {
        Self::LibCall(libcall)
    }
}

#[derive(Clone)]
pub struct BuiltinFunction {
    inner: Arc<BuiltinFunctionInner>,
}

impl BuiltinFunction {
    pub(crate) fn sig(&self) -> &ABISig {
        &self.inner.sig
    }

    pub(crate) fn ty(&self) -> BuiltinType {
        self.inner.ty
    }
}

#[macro_export]
macro_rules! foreach_builtin_function {
    ($mac:ident) => {
        $mac! {
            // Returns an index for wasm's `memory.grow` builtin function.
            memory32_grow(vmctx: vmctx, delta: i64, index: i32) -> pointer;
            // Returns an index for wasm's `table.copy` when both tables are locally
            // defined.
            table_copy(vmctx: vmctx, dst_index: i32, src_index: i32, dst: i64, src: i64, len: i64) -> bool;
            // Returns an index for wasm's `table.init`.
            table_init(vmctx: vmctx, table: i32, elem: i32, dst: i64, src: i64, len: i64) -> bool;
            // Returns an index for wasm's `elem.drop`.
            elem_drop(vmctx: vmctx, elem: i32);
            // Returns an index for wasm's `memory.copy`
            memory_copy(vmctx: vmctx, dst_index: i32, dst: i64, src_index: i32, src: i64, len: i64) -> bool;
            // Returns an index for wasm's `memory.fill` instruction.
            memory_fill(vmctx: vmctx, memory: i32, dst: i64, val: i32, len: i64) -> bool;
            // Returns an index for wasm's `memory.init` instruction.
            memory_init(vmctx: vmctx, memory: i32, data: i32, dst: i64, src: i32, len: i32) -> bool;
            // Returns a value for wasm's `ref.func` instruction.
            ref_func(vmctx: vmctx, func: i32) -> pointer;
            // Returns an index for wasm's `data.drop` instruction.
            data_drop(vmctx: vmctx, data: i32);
            // Returns a table entry after lazily initializing it.
            table_get_lazy_init_func_ref(vmctx: vmctx, table: i32, index: i64) -> pointer;
            // Returns an index for Wasm's `table.grow` instruction for `funcref`s.
            table_grow_func_ref(vmctx: vmctx, table: i32, delta: i64, init: pointer) -> pointer;
            // Returns an index for Wasm's `table.fill` instruction for `funcref`s.
            table_fill_func_ref(vmctx: vmctx, table: i32, dst: i64, val: pointer, len: i64) -> bool;
            // Returns an index for wasm's `memory.atomic.notify` instruction.
            #[cfg(feature = "threads")]
            memory_atomic_notify(vmctx: vmctx, memory: i32, addr: i64, count: i32) -> i64;
            // Returns an index for wasm's `memory.atomic.wait32` instruction.
            #[cfg(feature = "threads")]
            memory_atomic_wait32(vmctx: vmctx, memory: i32, addr: i64, expected: i32, timeout: i64) -> i64;
            // Returns an index for wasm's `memory.atomic.wait64` instruction.
            #[cfg(feature = "threads")]
            memory_atomic_wait64(vmctx: vmctx, memory: i32, addr: i64, expected: i64, timeout: i64) -> i64;
            // Invoked when fuel has run out while executing a function.
            out_of_gas(vmctx: vmctx) -> bool;
            // Invoked when we reach a new epoch.
            new_epoch(vmctx: vmctx) -> i64;
            // Invoked before malloc returns.
            #[cfg(feature = "wmemcheck")]
            check_malloc(vmctx: vmctx, addr: i32, len: i32) -> bool;
            // Invoked before the free returns.
            #[cfg(feature = "wmemcheck")]
            check_free(vmctx: vmctx, addr: i32) -> bool;
            // Invoked before a load is executed.
            #[cfg(feature = "wmemcheck")]
            check_load(vmctx: vmctx, num_bytes: i32, addr: i32, offset: i32) -> bool;
            // Invoked before a store is executed.
            #[cfg(feature = "wmemcheck")]
            check_store(vmctx: vmctx, num_bytes: i32, addr: i32, offset: i32) -> bool;
            // Invoked after malloc is called.
            #[cfg(feature = "wmemcheck")]
            malloc_start(vmctx: vmctx);
            // Invoked after free is called.
            #[cfg(feature = "wmemcheck")]
            free_start(vmctx: vmctx);
            // Invoked when wasm stack pointer is updated.
            #[cfg(feature = "wmemcheck")]
            update_stack_pointer(vmctx: vmctx, value: i32);
            // Invoked before memory.grow is called.
            #[cfg(feature = "wmemcheck")]
            update_mem_size(vmctx: vmctx, num_bytes: i32);

            // Drop a non-stack GC reference (eg an overwritten table entry)
            // once it will no longer be used again. (Note: `val` is not of type
            // `reference` because it needn't appear in any stack maps, as it
            // must not be live after this call.)
            #[cfg(feature = "gc-drc")]
            drop_gc_ref(vmctx: vmctx, val: i32);

            // Do a GC, treating the optional `root` as a GC root and returning
            // the updated `root` (so that, in the case of moving collectors,
            // callers have a valid version of `root` again).
            #[cfg(feature = "gc-drc")]
            gc(vmctx: vmctx, root: i32) -> i64;

            // Allocate a new, uninitialized GC object and return a reference to
            // it.
            #[cfg(feature = "gc-drc")]
            gc_alloc_raw(
                vmctx: vmctx,
                kind: i32,
                module_interned_type_index: i32,
                size: i32,
                align: i32
            ) -> i64;

            // Intern a `funcref` into the GC heap, returning its
            // `FuncRefTableId`.
            //
            // This libcall may not GC.
            #[cfg(feature = "gc")]
            intern_func_ref_for_gc_heap(
                vmctx: vmctx,
                func_ref: pointer
            ) -> i64;

            // Get the raw `VMFuncRef` pointer associated with a
            // `FuncRefTableId` from an earlier `intern_func_ref_for_gc_heap`
            // call.
            //
            // This libcall may not GC.
            //
            // Passes in the `ModuleInternedTypeIndex` of the funcref's expected
            // type, or `ModuleInternedTypeIndex::reserved_value()` if we are
            // getting the function reference as an untyped `funcref` rather
            // than a typed `(ref $ty)`.
            //
            // TODO: We will want to eventually expose the table directly to
            // Wasm code, so that it doesn't need to make a libcall to go from
            // id to `VMFuncRef`. That will be a little tricky: it will also
            // require updating the pointer to the slab in the `VMContext` (or
            // `VMRuntimeLimits` or wherever we put it) when the slab is
            // resized.
            #[cfg(feature = "gc")]
            get_interned_func_ref(
                vmctx: vmctx,
                func_ref_id: i32,
                module_interned_type_index: i32
            ) -> pointer;

            // Builtin implementation of the `array.new_data` instruction.
            #[cfg(feature = "gc")]
            array_new_data(
                vmctx: vmctx,
                array_interned_type_index: i32,
                data_index: i32,
                data_offset: i32,
                len: i32
            ) -> i64;

            // Builtin implementation of the `array.new_elem` instruction.
            #[cfg(feature = "gc")]
            array_new_elem(
                vmctx: vmctx,
                array_interned_type_index: i32,
                elem_index: i32,
                elem_offset: i32,
                len: i32
            ) -> i64;

            // Builtin implementation of the `array.copy` instruction.
            #[cfg(feature = "gc")]
            array_copy(
                vmctx: vmctx,
                dst_array: i32,
                dst_index: i32,
                src_array: i32,
                src_index: i32,
                len: i32
            ) -> bool;

            // Builtin implementation of the `array.init_data` instruction.
            #[cfg(feature = "gc")]
            array_init_data(
                vmctx: vmctx,
                array_interned_type_index: i32,
                array: i32,
                dst_index: i32,
                data_index: i32,
                data_offset: i32,
                len: i32
            ) -> bool;

            // Builtin implementation of the `array.init_elem` instruction.
            #[cfg(feature = "gc")]
            array_init_elem(
                vmctx: vmctx,
                array_interned_type_index: i32,
                array: i32,
                dst: i32,
                elem_index: i32,
                src: i32,
                len: i32
            ) -> bool;

            // Returns whether `actual_engine_type` is a subtype of
            // `expected_engine_type`.
            #[cfg(feature = "gc")]
            is_subtype(
                vmctx: vmctx,
                actual_engine_type: i32,
                expected_engine_type: i32
            ) -> i32;

            // Returns an index for Wasm's `table.grow` instruction for GC references.
            #[cfg(feature = "gc")]
            table_grow_gc_ref(vmctx: vmctx, table: i32, delta: i64, init: i32) -> pointer;

            // Returns an index for Wasm's `table.fill` instruction for GC references.
            #[cfg(feature = "gc")]
            table_fill_gc_ref(vmctx: vmctx, table: i32, dst: i64, val: i32, len: i64) -> bool;

            // Raises an unconditional trap with the specified code.
            //
            // This is used when signals-based-traps are disabled for backends
            // when an illegal instruction can't be executed for example.
            trap(vmctx: vmctx, code: u8);

            // Raises an unconditional trap where the trap information must have
            // been previously filled in.
            raise(vmctx: vmctx);
        }
    };
}

/// Metadata about a builtin function.
pub struct BuiltinFunctionInner {
    /// The ABI specific signature of the function.
    sig: ABISig,
    /// The built-in function type.
    ty: BuiltinType,
}

macro_rules! declare_function_sig {

    (
        $(
            $( #[$attr:meta] )*
            $name:ident( $( $pname:ident: $param:ident ),* ) $( -> $result:ident )?;
         )*
    ) 
    
    =>
    
    {
        /// Provides the ABI signatures for each builtin function
        /// signature.
        pub struct BuiltinFunctions {
            /// The target calling convention for host intrinsics.
            host_call_conv: CallingConvention,
            /// The target calling convention for wasm builtins.
            wasm_call_conv: CallingConvention,
            /// The target pointer type, as a WebAssembly type.
            ptr_type: WasmValType,
            /// F32 Ceil.
            ceil_f32: Option<BuiltinFunction>,
            /// F64 Ceil.
            ceil_f64: Option<BuiltinFunction>,
            /// F32 Floor.
            floor_f32: Option<BuiltinFunction>,
            /// F64 Floor.
            floor_f64: Option<BuiltinFunction>,
            /// F32 Trunc.
            trunc_f32: Option<BuiltinFunction>,
            /// F64 Trunc.
            trunc_f64: Option<BuiltinFunction>,
            /// F32 Nearest.
            nearest_f32: Option<BuiltinFunction>,
            /// F64 Nearest.
            nearest_f64: Option<BuiltinFunction>,
            $(
                $( #[ $attr ] )*
                $name: Option<BuiltinFunction>,
            )*
        }

        // Until all the builtin functions are used.
        #[allow(dead_code)]
        impl BuiltinFunctions {
            pub fn new<P: PtrSize>(
                vmoffsets: &VMOffsets<P>,
                host_call_conv: CallingConvention,
                wasm_call_conv: CallingConvention,
            ) -> Self {
                let size = vmoffsets.ptr.size();
                #[allow(unused_doc_comments)]
                Self {
                    host_call_conv,
                    wasm_call_conv,
                    ptr_type: ptr_type_from_ptr_size(size),
                    ceil_f32: None,
                    ceil_f64: None,
                    floor_f32: None,
                    floor_f64: None,
                    trunc_f32: None,
                    trunc_f64: None,
                    nearest_f32: None,
                    nearest_f64: None,
                    $(
                        $( #[ $attr ] )*
                        $name: None,
                    )*
                }
            }

            fn pointer(&self) -> WasmValType {
                self.ptr_type
            }

            fn vmctx(&self) -> WasmValType {
                self.pointer()
            }

            fn i32(&self) -> WasmValType {
                WasmValType::I32
            }

            fn u8(&self) -> WasmValType {
                WasmValType::I32
            }

            fn f32(&self) -> WasmValType {
                WasmValType::F32
            }

            fn f64(&self) -> WasmValType {
                WasmValType::F64
            }

            fn i64(&self) -> WasmValType {
                WasmValType::I64
            }

            fn bool(&self) -> WasmValType {
                WasmValType::I32
            }

            fn over_f64<A: ABI>(&self) -> ABISig {
                A::sig_from(&[self.f64()], &[self.f64()], &self.host_call_conv)
            }

            fn over_f32<A: ABI>(&self) -> ABISig {
                A::sig_from(&[self.f64()], &[self.f64()], &self.host_call_conv)
            }

            pub(crate) fn ceil_f32<A: ABI>(&mut self) -> BuiltinFunction {
                if self.ceil_f32.is_none() {
                    let sig = self.over_f32::<A>();
                    let inner = Arc::new(BuiltinFunctionInner { sig, ty: BuiltinType::libcall(LibCall::CeilF32) });
                    self.ceil_f32 = Some(BuiltinFunction {
                        inner,
                    });
                }
                self.ceil_f32.as_ref().unwrap().clone()
            }

            pub(crate) fn ceil_f64<A: ABI>(&mut self) -> BuiltinFunction {
                if self.ceil_f64.is_none() {
                    let sig = self.over_f64::<A>();
                    let inner = Arc::new(BuiltinFunctionInner { sig, ty: BuiltinType::libcall(LibCall::CeilF64) });
                    self.ceil_f64 = Some(BuiltinFunction {
                        inner,
                    });
                }
                self.ceil_f64.as_ref().unwrap().clone()
            }

            pub(crate) fn floor_f32<A: ABI>(&mut self) -> BuiltinFunction {
                if self.floor_f32.is_none() {
                    let sig = self.over_f32::<A>();
                    let inner = Arc::new(BuiltinFunctionInner { sig, ty: BuiltinType::libcall(LibCall::FloorF32) });
                    self.floor_f32 = Some(BuiltinFunction {
                        inner,
                    });
                }
                self.floor_f32.as_ref().unwrap().clone()
            }

            pub(crate) fn floor_f64<A: ABI>(&mut self) -> BuiltinFunction {
                if self.floor_f64.is_none() {
                    let sig = self.over_f64::<A>();
                    let inner = Arc::new(BuiltinFunctionInner { sig, ty: BuiltinType::libcall(LibCall::FloorF64) });
                    self.floor_f64 = Some(BuiltinFunction {
                        inner,
                    });
                }
                self.floor_f64.as_ref().unwrap().clone()
            }

            pub(crate) fn trunc_f32<A: ABI>(&mut self) -> BuiltinFunction {
                if self.trunc_f32.is_none() {
                    let sig = self.over_f32::<A>();
                    let inner = Arc::new(BuiltinFunctionInner { sig, ty: BuiltinType::libcall(LibCall::TruncF32) });
                    self.trunc_f32 = Some(BuiltinFunction {
                        inner,
                    });
                }
                self.trunc_f32.as_ref().unwrap().clone()
            }

            pub(crate) fn trunc_f64<A: ABI>(&mut self) -> BuiltinFunction {
                if self.trunc_f64.is_none() {
                    let sig = self.over_f64::<A>();
                    let inner = Arc::new(BuiltinFunctionInner { sig, ty: BuiltinType::libcall(LibCall::TruncF64) });
                    self.trunc_f64 = Some(BuiltinFunction {
                        inner,
                    });
                }
                self.trunc_f64.as_ref().unwrap().clone()
            }

            pub(crate) fn nearest_f32<A: ABI>(&mut self) -> BuiltinFunction {
                if self.nearest_f32.is_none() {
                    let sig = self.over_f32::<A>();
                    let inner = Arc::new(BuiltinFunctionInner { sig, ty: BuiltinType::libcall(LibCall::NearestF32) });
                    self.nearest_f32 = Some(BuiltinFunction {
                        inner,
                    });
                }
                self.nearest_f32.as_ref().unwrap().clone()
            }

            pub(crate) fn nearest_f64<A: ABI>(&mut self) -> BuiltinFunction {
                if self.nearest_f64.is_none() {
                    let sig = self.over_f64::<A>();
                    let inner = Arc::new(BuiltinFunctionInner { sig, ty: BuiltinType::libcall(LibCall::NearestF64) });
                    self.nearest_f64 = Some(BuiltinFunction {
                        inner,
                    });
                }
                self.nearest_f64.as_ref().unwrap().clone()
            }

            $(
                $( #[ $attr ] )*
                pub(crate) fn $name<A: ABI, P: PtrSize>(&mut self) -> BuiltinFunction {
                    if self.$name.is_none() {
                        let params = vec![ $(self.$param() ),* ];
                        let result = vec![ $(self.$result() )?];
                        let sig = A::sig_from(&params, &result, &self.wasm_call_conv);
                        let index = BuiltinFunctionIndex::$name();
                        let inner = Arc::new(BuiltinFunctionInner { sig, ty: BuiltinType::builtin(index) });
                        self.$name = Some(BuiltinFunction {
                            inner,
                        });
                    }

                    self.$name.as_ref().unwrap().clone()
                }
             )*
        }
    }
}

foreach_builtin_function!(declare_function_sig);