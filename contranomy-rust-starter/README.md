# Rust starter

This directory contains a Rust project that builds a RISC-V ELF file suitable
for simulation via contranomy.

## Files

The `memory.x` file is a small linker file that describes the layout of the
target machine (in the simplest case just a certain range of RAM and sections
mapped to said RAM).

The `build.rs` file makes sure that any additionally files (for example the
`memory.x` file) are available and findable by the Rust compiler when building.
