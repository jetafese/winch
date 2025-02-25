# Default values for the paths, which can be overridden by command-line arguments
SEA ?= ../../seahorn/bin/sea
YAML ?= sea.yaml
LLVM_FILE ?= $$(ls ../target/release/deps/winch_codegen*.ll) 
# ../target/release/deps/winch_codegen-0f5d73a86810f0eb.ll

all: verify

verify:
	$(SEA) yama -y $(YAML) bpf $(LLVM_FILE)
