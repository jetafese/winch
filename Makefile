# Default values for the paths, which can be overridden by command-line arguments
SEA ?= ../seahorn/bin/sea
YAML ?= verify/sea.yaml
LLVM_FILE ?= $$(ls target/release/deps/winch_codegen*.ll) 

.PHONY: all verify

all: clean build verify

build:
	RUSTFLAGS=--emit=llvm-ir cargo build --release

verify:
	$(SEA) yama -y $(YAML) bpf $(LLVM_FILE)

clean:
	cargo clean