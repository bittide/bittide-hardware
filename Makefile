GHC=ghc-9.0.2

.PHONY: build-sim
build-sim:
	cd contranomy; cabal build simcontranomy -w $(GHC)


.PHONY: build-rust-starter
build-rust-starter:
	cd contranomy-rust-starter; cargo build --release

.PHONY: sim-rust-starter
sim-rust-starter: build-sim build-rust-starter
	cp contranomy-rust-starter/target/riscv32imc-unknown-none-elf/release/contranomy-rust-starter contranomy/main.elf
	cd contranomy; cabal run simcontranomy -w $(GHC)
