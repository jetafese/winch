# Default values for the paths, which can be overridden by command-line arguments

rust-host := `rustc -vV | sed -n 's|host: ||p'`

all: clean build verify

build:
	RUSTFLAGS=--emit=llvm-ir cargo build --release --target={{rust-host}}

verify SEA='../seahorn/bin/sea' YAML='verify/sea.yaml':
	{{SEA}} yama -y {{YAML}} bpf `ls target/{{rust-host}}/release/deps/winch_codegen*.ll | head -n 1`

clean:
	cargo clean

debug SEA_LIB='../seahorn/lib/libsea-rt.a':
	clang++ `ls target/{{rust-host}}/release/deps/winch_codegen*.ll | head -n 1` /tmp/winch/h.ll {{SEA_LIB}} -o exec.out