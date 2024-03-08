# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: CC0-1.0

CPU_DIR = example-cpu
OUT_DIR = build_out_dir
VERILATOR_DIR = $(OUT_DIR)/verilator_output
FFI_DIR = src/ffi

VERILATOR_FLAGS = -CFLAGS '-O3 -fPIC' -Wno-fatal +1364-2001ext+v --trace

VERILATOR_CFLAGS = $(shell pkg-config --cflags verilator)
FFI_CPPFLAGS = $(VERILATOR_CFLAGS) -fPIC -O3 -I$(VERILATOR_DIR)

all: $(OUT_DIR)/libVexRiscvFFI.a # $(OUT_DIR)/libVexRiscvFFI.so

clean:
	rm $(VERILATOR_DIR) -rf
	rm $(CPU_DIR)/VexRiscv.v -rf
	rm $(OUT_DIR) -rf

$(CPU_DIR)/VexRiscv.v: $(CPU_DIR)/src/main/scala/example/ExampleCpu.scala
	cd $(CPU_DIR); sbt "runMain example.ExampleCpu"
	cd $(CPU_DIR); sed -i -E '/\/\/ Git hash  :.*$$/d' VexRiscv.v

$(OUT_DIR)/VexRiscv.v: $(CPU_DIR)/VexRiscv.v
	mkdir -p $(OUT_DIR)
	cp $(CPU_DIR)/VexRiscv.v $(OUT_DIR)/VexRiscv.v

$(VERILATOR_DIR)/VVexRiscv.mk $(VERILATOR_DIR)/VVexRiscv.h: $(OUT_DIR)/VexRiscv.v
	cd $(OUT_DIR); verilator $(VERILATOR_FLAGS) --cc -Mdir ../$(VERILATOR_DIR) VexRiscv.v

$(VERILATOR_DIR)/VVexRiscv__ALL.a: $(VERILATOR_DIR)/VVexRiscv.mk
	cd $(VERILATOR_DIR); make -f VVexRiscv.mk

$(OUT_DIR)/impl.o: $(FFI_DIR)/impl.cpp $(FFI_DIR)/interface.h
	$(CXX) $(FFI_CPPFLAGS) -c $(FFI_DIR)/impl.cpp -o $(OUT_DIR)/impl.o

$(OUT_DIR)/verilated_vcd_c.o: $(shell pkg-config --variable=includedir verilator)/verilated_vcd_c.cpp
	$(CXX) $(FFI_CPPFLAGS) -c $(shell pkg-config --variable=includedir verilator)/verilated_vcd_c.cpp -o $(OUT_DIR)/verilated_vcd_c.o


$(OUT_DIR)/verilated.o: $(shell pkg-config --variable=includedir verilator)/verilated.cpp
	$(CXX) $(FFI_CPPFLAGS) -c $(shell pkg-config --variable=includedir verilator)/verilated.cpp -o $(OUT_DIR)/verilated.o

$(OUT_DIR)/verilated_threads.o: $(shell pkg-config --variable=includedir verilator)/verilated_threads.cpp
	$(CXX) $(FFI_CPPFLAGS) -c $(shell pkg-config --variable=includedir verilator)/verilated_threads.cpp -o $(OUT_DIR)/verilated_threads.o

$(OUT_DIR)/VVexRiscv__ALL.a: $(VERILATOR_DIR)/VVexRiscv__ALL.a
	cp $(VERILATOR_DIR)/VVexRiscv__ALL.a $(OUT_DIR)/VVexRiscv__ALL.a

$(OUT_DIR)/libVexRiscvFFI.a: $(OUT_DIR)/VVexRiscv__ALL.a $(OUT_DIR)/impl.o $(OUT_DIR)/verilated.o $(OUT_DIR)/verilated_threads.o $(OUT_DIR)/verilated_vcd_c.o
	rm -f $(OUT_DIR)/libVexRiscvFFI.a
	cp $(OUT_DIR)/VVexRiscv__ALL.a $(OUT_DIR)/libVexRiscvFFI.a
	ar r \
		$(OUT_DIR)/libVexRiscvFFI.a \
		$(OUT_DIR)/impl.o \
		$(OUT_DIR)/verilated.o \
		$(OUT_DIR)/verilated_threads.o \
		$(OUT_DIR)/verilated_vcd_c.o

$(OUT_DIR)/libVexRiscvFFI.so: $(OUT_DIR)/libVexRiscvFFI.a
	rm -f $(OUT_DIR)/libVexRiscvFFI.so
	$(CXX) -shared -o $(OUT_DIR)/libVexRiscvFFI.so \
		-Wl,--whole-archive \
			$(OUT_DIR)/libVexRiscvFFI.a \
		-Wl,--no-whole-archive
