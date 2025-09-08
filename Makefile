# SLOW-32 Project Makefile

.PHONY: all clean emulator assembler linker runtime test tools

all: tools runtime

tools:
	$(MAKE) -C tools

emulator:
	$(MAKE) -C tools/emulator

assembler:
	$(MAKE) -C tools/assembler

linker:
	$(MAKE) -C tools/linker

runtime: tools
	$(MAKE) -C runtime

test: all
	@echo "=== Running basic tests ==="
	@echo "Testing assembler..."
	@echo "addi r1, r0, 42" > /tmp/test.s
	@echo "halt" >> /tmp/test.s
	@./tools/assembler/slow32asm /tmp/test.s /tmp/test.s32o
	@echo "Testing emulator..."
	@./tools/linker/s32-ld -o /tmp/test.s32x /tmp/test.s32o
	@./tools/emulator/slow32 /tmp/test.s32x | grep "r01=0x0000002A" && echo "✓ Basic test passed"

benchmark: emulator
	@if [ -f tests/binaries/bench.bin ]; then \
		echo "=== Benchmarking emulators ==="; \
		echo "Standard:"; \
		time ./emulator/slow32 tests/binaries/bench.bin > /dev/null 2>&1; \
		echo "Optimized:"; \
		time ./emulator/slow32_fast tests/binaries/bench.bin > /dev/null 2>&1; \
	else \
		echo "Benchmark binary not found. Run 'make test' first."; \
	fi

clean:
	$(MAKE) -C tools clean
	$(MAKE) -C runtime clean
	rm -f *.o *.bin *.ll *.s *.s32o *.s32x

help:
	@echo "SLOW-32 Build System"
	@echo "===================="
	@echo "Targets:"
	@echo "  all       - Build everything"
	@echo "  emulator  - Build the CPU emulator"
	@echo "  assembler - Build the assembler"
	@echo "  compiler  - Build the LLVM compiler"
	@echo "  test      - Run basic tests"
	@echo "  benchmark - Compare emulator performance"
	@echo "  clean     - Remove all build artifacts"
	@echo "  help      - Show this message"