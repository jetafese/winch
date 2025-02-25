## ASM Verification

Winch is a WebAssembly "baseline" or single-pass compiler designed for Wasmtime. Our primary goal in this branch is to successfully verify the assembler in Winch. This is done by stubbing the emitter and machine buffer so that we can focus on assembler functions. Note that we are focusing on the x86_64 target in our verification effort.

## Dependencies

LLVM 14, SeaHorn

## Usage

The objective is to compile the winch project main file, which defines our unit proofs, and give the generated llvm-ir to SeaHorn for verification. To achieve this, we generate the llvm-ir with the command:


`RUSTFLAGS=--emit=llvm-ir cargo build --release 2>&1 | head -n 10`

There should be a set of files of the form `winch_codegen-*.ll` in the `target/release/deps` directory. One of them will be relatively empty, but the other should have the main function and the functions we have defined.

Once we have the desired llvm-ir (say `winch_codegen-2.ll`), we give it to SeaHorn with the command:

`/path-to-seahorn/build/run/bin/sea yama -y sea.yaml bpf target/release/deps/winch_codegen-2.ll`


## Expected Results 

At the moment, we expect to get unsat since none of the functions have a path that leads to a panic. If you are interested in catching an intentionally planted panic, add one in the call path of the functions in our unit proof. For example, adding a panic in the initialization of the Machine Buffer will result in a panic detection in our unit proof.

## Docker

There is a docker setup that can be accessed in `verify`. It can be built and run with the commands `docker build -t winches . --file Dockerfile` and `docker run -it winch` respectively. 

The `make` command can be used to run SeaHorn on the generated files. If you run into an issue where there are multiple files generated, specify the one you are interested in as follows: `make LLVM_FILE=../target/release/deps/winch_codegen{file-of-interest}.ll`. The same can be done for the other parameters in the Makefile.