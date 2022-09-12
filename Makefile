# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: CC0-1.0

CARGO_TARGET_DIR=target

.PHONY: build-sim
build-sim:
	cabal build simcontranomy


.PHONY: contranomy-tests
contranomy-tests: build-sim copy-firmware-tests
	cabal run contranomy:unittests
	cabal run contranomy-sim:unittests


.PHONY: build-firmware-tests
build-firmware-tests:
	cd firmware/tests; cargo build --release

.PHONY: copy-firmware-tests
copy-firmware-tests: build-firmware-tests
	rm -rf firmware-integration-tests
	mkdir firmware-integration-tests

	# Copy artifacts into "clean" folder
	cd firmware/tests; cat target/artifacts | xargs -i cp ./{} ../../firmware-integration-tests/




.PHONY: build-firmware-example-hello
build-firmware-example-hello:
	cd firmware/examples/hello; cargo build --release --target-dir ../../../$(CARGO_TARGET_DIR)


.PHONY: sim-firmware-example-hello
sim-firmware-example-hello: build-sim build-firmware-example-hello
	cabal run simcontranomy -- target/riscv32imc-unknown-none-elf/release/hello



.PHONY: build-firmware-example-fdt-read
build-firmware-example-fdt-read:
	cd firmware/examples/fdt-read; cargo build --release --target-dir ../../../$(CARGO_TARGET_DIR)

.PHONY: sim-firmware-example-fdt-read
sim-firmware-example-fdt-read: build-sim build-firmware-example-fdt-read
	cabal run simcontranomy -- target/riscv32imc-unknown-none-elf/release/fdt-read
