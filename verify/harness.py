import re
import subprocess
import argparse

error_file = "error.txt"
harness_file = "harness.ll"

# Extract undefined symbols from the error log
def extract_undefined_symbols(log_content):
    pattern = r'["\'](_+[\w\d]+)["\']'
    return set(re.findall(pattern, log_content))

# Generate LLVM function definition
def generate_definition(symbol):
    # Strip leading underscores from the symbol name
    symbol = symbol.lstrip('_')
    sym_name = f"@__{symbol}"
    if "i64" in symbol or "u64" in symbol:
        ret_type = "i64"
        ret_value = "0"
    elif "i32" in symbol or "u32" in symbol:
        ret_type = "i32"
        ret_value = "0"
    elif "i8" in symbol or "u8" in symbol:
        ret_type = "i8"
        ret_value = "0"
    else:
        ret_type = "void"
        ret_value = ""
        sym_name = f"@{symbol}"
    
    return (
        f"define {ret_type} {sym_name}() {{\n"
        f"  ret {ret_type} {ret_value}\n"
        f"}}\n"
    )

def run_clang(proof, sea_lib, file, harness=''):
    # Construct the clang++ command
    clang_command = [
        "clang++", file,
        f"/tmp/winch/{proof}.ll",   # seahorn counter example
        sea_lib,                    # Use the sea_lib variable
        harness,                    # generated harness file
        "-o", "exec.out"            # executable
    ]
    
    # Run the command and capture output and errors
    result = subprocess.run(clang_command, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    if result.returncode != 0:
        # If Clang fails, print the error and save to {error_file}
        with open(error_file, "wb") as f:
            f.write(result.stderr)
        print(f"Clang failed. Check {error_file} for details.")
    else:
        print("Clang command executed successfully. Created exec.out.")
    
    return result.returncode
    

# Main function
def main(proof, sea_lib, file):
    # Run Clang command
    res = run_clang(proof, sea_lib, file)

    if res == 0:
        return

    # Read the linker error log
    with open(error_file, 'r') as f:
        error_content = f.read()

    # Extract and generate definitions
    symbols = extract_undefined_symbols(error_content)
    definitions = [generate_definition(sym) for sym in sorted(symbols)]

    # Write to {harness_file} (overwrite)
    with open(harness_file, 'w') as f:
        f.write("; === Auto-generated symbol definitions ===\n\n")
        for defn in definitions:
            f.write(defn + "\n")

    print(f"Written {len(definitions)} definitions to {harness_file}.")

    # run clang command with new harness file
    run_clang(proof, sea_lib, file, f"{harness_file}")

# Set up argument parsing
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate function definitions and run Clang command.")
    parser.add_argument("proof", type=str, help="The proof name (e.g., 'visit_setup')")
    parser.add_argument("sea_lib", type=str, help="Path to the SEA_LIB library (e.g., '../seahorn/build/run/lib/libsea-rt.a')")
    parser.add_argument("file", type=str, help="Rust host architecture (e.g., 'target/aarch64-apple-darwin/release/deps/visit_funcs-3c64056cce5c6e1e.ll')")
    
    args = parser.parse_args()
    
    # Run the main function with input variables
    main(args.proof, args.sea_lib, args.file)
