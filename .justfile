# Default values for the paths, which can be overridden by command-line arguments

rust-host := `rustc -vV | sed -n 's|host: ||p'`

all PROOF='visit_setup':
	cargo clean
	just prove {{PROOF}}

prove PROOF='visit_setup':
	just build {{PROOF}}
	just verify {{PROOF}} 

bproof PROOF='visit_setup' BOUND='2':
	just build {{PROOF}}
	just bound {{PROOF}} {{BOUND}}

build PROOF='visit_setup':
	RUSTFLAGS=--emit=llvm-ir cargo build --release --target={{rust-host}} --bin {{PROOF}}

verify PROOF='visit_setup' SEA='../seahorn/bin/sea' YAML='verify/sea.yaml':
	{{SEA}} yama -y {{YAML}} bpf `ls target/{{rust-host}}/release/deps/{{PROOF}}*.ll | head -n 1` --cex=/tmp/winch/{{PROOF}}.ll

bound PROOF='visit_setup' BOUND='2' SEA='../seahorn/bin/sea' YAML='verify/sea.yaml':
	{{SEA}} yama -y {{YAML}} bpf `ls target/{{rust-host}}/release/deps/{{PROOF}}*.ll | head -n 1` --cex=/tmp/winch/{{PROOF}}.ll --bound={{BOUND}}

clean:
	cargo clean
	rm -f exec.out

debug PROOF='visit_setup' SEA_LIB='../seahorn/lib/libsea-rt.a':
	rm -f exec.out
	clang++ `ls target/{{rust-host}}/release/deps/{{PROOF}}*.ll | head -n 1` /tmp/winch/{{PROOF}}.ll {{SEA_LIB}} -o exec.out