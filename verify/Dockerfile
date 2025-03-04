FROM seahorn/seahorn-llvm14:nightly
# FROM rust:1.64-bullseye


USER root

RUN apt-get update && apt-get install -y \
build-essential \
clang \
cmake \
pkg-config \
libssl-dev \
sudo \
&& rm -rf /var/lib/apt/lists/*

# Install rustup
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
# Add .cargo/bin to PATH
ENV PATH="/root/.cargo/bin:${PATH}"

# install the just command runner
# create ~/bin
RUN mkdir -p ~/bin
# download and extract just to ~/bin/just
RUN curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to ~/bin
# add `~/bin` to the paths that your shell searches for executables
RUN export PATH="$PATH:$HOME/bin"

# USER usea
WORKDIR /home/usea

# Set Rust toolchain to nightly-2022-08-01
RUN rustup install nightly-2022-08-01 && \
rustup default nightly-2022-08-01 && \
rustup component add rustfmt clippy

RUN git clone https://github.com/jetafese/winch/
WORKDIR /home/usea/winch/
RUN git checkout codegen
# try to cache some of the work cargo does:
RUN sh -c 'find . -mindepth 1 ! -name "Cargo.toml" ! -name "Cargo.lock" ! -path "./.git*" -exec rm -rf {} +'
RUN mkdir -p src && echo "fn main() {}" > src/main.rs
RUN sed -i '/seahorn/d' Cargo.toml
RUN sed -i '/libc/d' Cargo.toml
RUN RUSTFLAGS=--emit=llvm-ir cargo build --release
RUN rm -rf src
RUN git reset --hard HEAD