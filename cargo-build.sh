#!/bin/sh

echo "Checking for an existing version of cargo..."

echo "+ cargo +nightly --version"
if cargo +nightly --version; then \
    echo "Cargo found! Using system installation of cargo."; \
else \
    echo "Not found. Downloading cargo..."; \

    # Make sure the rust compiler is installed locally in the project directory.
    export RUSTUP_HOME="$(pwd)/.rustup"
    export CARGO_HOME="$(pwd)/.cargo"

    # Yes, this looks sketchy, but it's the official way of installing the rust compiler.
    # For reference, see https://rustup.rs/
    curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain nightly -y --no-modify-path

    . .cargo/env
fi;

set -x
cargo +nightly build --locked
