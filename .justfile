# Default values for the paths, which can be overridden by command-line arguments

all: clean build verify

build:
	RUSTFLAGS=--emit=llvm-ir cargo build --release

verify SEA='../seahorn/bin/sea' \
	YAML='verify/sea.yaml' \
	LLVM_FILE='`ls target/release/deps/winch_codegen*.ll`':
	{{SEA}} yama -y {{YAML}} bpf {{LLVM_FILE}}

clean:
	cargo clean

debug LLVM_FILE='`ls target/release/deps/winch_codegen*.ll`' \
	SEA_LIB='../seahorn/lib/libsea-rt.a':
	clang++ {{LLVM_FILE}} /tmp/winch/h.ll {{SEA_LIB}} -o exec.out