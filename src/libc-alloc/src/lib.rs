// MIT License

// Copyright (c) 2022 Daniel Prilik
// Copyright (c) 2023 Giuliano Losa

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// We need a way to allocate memory that is supported by Seahorn.
// Seahorn only supports malloc, free, and alloca

#![no_std]

use core::alloc::{GlobalAlloc, Layout};
use core::ffi::c_void;
use core::ptr;
use core::cmp;

#[cfg(target_family = "unix")]
extern crate libc;

/// Global Allocator which hooks into libc to allocate / free memory.
pub struct LibcAlloc;

#[cfg(target_family = "unix")]
unsafe impl GlobalAlloc for LibcAlloc {
    #[inline]
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // TODO: the original code uses posix_memalign; is it okay to use malloc?
        libc::malloc(layout.size()) as *mut u8
    }

    #[inline]
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let ptr = self.alloc(layout);
        if !ptr.is_null() {
            ptr::write_bytes(ptr, 0, layout.size());
        }
        ptr
    }

    #[inline]
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        libc::free(ptr as *mut c_void);
    }

    #[inline]
    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        // Seahorn does not support libc::realloc
        // TODO: is this okay?
        let new_ptr = libc::malloc(new_size) as *mut u8;
        let num_to_copy = cmp::min(new_size, layout.size());
        ptr::copy_nonoverlapping(ptr, new_ptr, num_to_copy);
        new_ptr
    }
}


