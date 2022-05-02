# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: CC0-1.0

CARGO_TARGET_DIR=target

.PHONY: build-sim
build-sim:
	cd contranomy-sim; cabal build simcontranomy


.PHONY: build-firmware-example-hello
build-firmware-example-hello:
	cd firmware/examples/hello; cargo build --release --target-dir ../../../$(CARGO_TARGET_DIR)

.PHONY: sim-firmware-example-hello
sim-firmware-example-hello: build-sim build-firmware-example-hello
	cp target/riscv32imc-unknown-none-elf/release/hello contranomy-sim/main.elf
	cd contranomy-sim; cabal run simcontranomy
