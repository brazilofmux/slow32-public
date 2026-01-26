# SLOW-32 Project Makefile

# Tools and paths
LLVM_BIN := ~/llvm-project/build/bin
CLANG := $(LLVM_BIN)/clang
LLC := $(LLVM_BIN)/llc
ASM := tools/assembler/slow32asm
LD := tools/linker/s32-ld
EMU := tools/emulator/slow32

# Default flags
TARGET := slow32-unknown-none
OPT := -O2
CFLAGS := -target $(TARGET) -S -emit-llvm $(OPT) -Iruntime/include

# Default libc version (can be overridden with LIBC=mmio)
LIBC ?= debug
LIBC_ARCHIVE := runtime/libc_$(LIBC).s32a

.PHONY: all clean emulator assembler compiler runtime test tools cpp-test cpp-run dbt

all: tools compiler runtime

tools: emulator assembler linker utilities dbt

emulator:
	$(MAKE) -C tools/emulator

assembler:
	$(MAKE) -C tools/assembler

linker:
	$(MAKE) -C tools/linker

utilities:
	$(MAKE) -C tools/utilities

dbt:
	$(MAKE) -C tools/dbt

runtime: assembler
	$(MAKE) -C runtime

test: all
	@echo "=== Running basic tests ==="
	@echo "Testing assembler..."
	@echo "addi r1, r0, 42" > /tmp/test.s
	@echo "halt" >> /tmp/test.s
	@./tools/assembler/slow32asm /tmp/test.s /tmp/test.s32o
	@./tools/linker/s32-ld -o /tmp/test.s32x /tmp/test.s32o
	@echo "Testing emulator..."
	@./tools/emulator/slow32 -r /tmp/test.s32x | grep "r1: 0x00000000 -> 0x0000002A" && echo "✓ Basic test passed"

cpp-test: tools
	@./examples/cpp/test_all.sh

cpp-run: all
	@./examples/cpp/build_and_run.sh

benchmark: emulator
	@if [ -f tests/binaries/bench.bin ]; then \
		echo "=== Benchmarking emulators ==="; \
		echo "Standard:"; \
		time ./tools/emulator/slow32 tests/binaries/bench.bin > /dev/null 2>&1; \
		echo "Optimized:"; \
		time ./tools/emulator/slow32_fast tests/binaries/bench.bin > /dev/null 2>&1; \
	else \
		echo "Benchmark binary not found. Run 'make test' first."; \
	fi

clean:
	$(MAKE) -C tools/emulator clean
	$(MAKE) -C tools/assembler clean
	$(MAKE) -C tools/linker clean
	$(MAKE) -C tools/utilities clean
	$(MAKE) -C tools/dbt clean
	$(MAKE) -C runtime clean
	rm -rf tests/
	rm -f *.o *.bin *.ll *.s *.s32o *.s32x
	rm -f benchmark* assembler_reloc_patch.txt
	find . -name "*.s32o" -o -name "*.s32x" | grep -v runtime | grep -v regression | xargs rm -f

# C compilation pattern rules
%.ll: %.c runtime
	@echo "Compiling $< to LLVM IR..."
	$(CLANG) $(CFLAGS) $< -o $@

%.s: %.ll
	@echo "Generating assembly for $<..."
	$(LLC) -mtriple=$(TARGET) $< -o $@

%.s32o: %.s
	@echo "Assembling $<..."
	$(ASM) $< $@

# Link a C program (with optional MMIO support)
%.s32x: %.s32o runtime
	@echo "Linking $< (using $(LIBC) libc)..."
	@if [ "$(LIBC)" = "mmio" ]; then \
		$(LD) --mmio 64K -o $@ runtime/crt0.s32o $< $(LIBC_ARCHIVE) runtime/libs32.s32a; \
	else \
		$(LD) -o $@ runtime/crt0.s32o $< $(LIBC_ARCHIVE) runtime/libs32.s32a; \
	fi
	@echo "Success! Run with: $(EMU) $@"

# Run a program
run-%: %.s32x
	$(EMU) $<

help:
	@echo "SLOW-32 Build System"
	@echo "===================="
	@echo "Main Targets:"
	@echo "  all       - Build everything"
	@echo "  emulator  - Build the CPU emulator"
	@echo "  assembler - Build the assembler"
	@echo "  compiler  - Build the LLVM compiler"
	@echo "  dbt       - Build the dynamic binary translator"
	@echo "  runtime   - Build runtime libraries"
	@echo "  test      - Run basic tests"
	@echo "  cpp-test  - Compile C++ examples (no execution)"
	@echo "  cpp-run   - Build and run C++ examples"
	@echo "  benchmark - Compare emulator performance"
	@echo "  clean     - Remove all build artifacts"
	@echo "  help      - Show this message"
	@echo ""
	@echo "C Compilation:"
	@echo "  %.s32x    - Compile and link a C file (e.g., make test.s32x)"
	@echo "  run-%     - Compile and run a C file (e.g., make run-test)"
	@echo ""
	@echo "Options:"
	@echo "  LIBC=debug  - Use DEBUG instruction for I/O (default)"
	@echo "  LIBC=mmio   - Use MMIO ring buffer for I/O"
	@echo ""
	@echo "Example:"
	@echo "  make test.s32x           # Compile test.c with DEBUG I/O"
	@echo "  make test.s32x LIBC=mmio # Compile test.c with MMIO I/O"
	@echo "  make run-test            # Compile and run test.c"
