# SLOW-32 Project Makefile

.PHONY: all clean emulator assembler compiler runtime test

all: emulator assembler compiler runtime

emulator:
	$(MAKE) -C emulator

assembler:
	$(MAKE) -C assembler

compiler:
	$(MAKE) -C llvm-backend/standalone

runtime: assembler
	$(MAKE) -C runtime

test: all
	@echo "=== Running basic tests ==="
	@echo "Testing assembler..."
	@echo "addi r1, r0, 42" > /tmp/test.s
	@echo "halt" >> /tmp/test.s
	@./assembler/slow32asm /tmp/test.s /tmp/test.bin
	@echo "Testing emulator..."
	@./emulator/slow32 /tmp/test.bin | grep "r01=0x0000002A" && echo "✓ Basic test passed"

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
	$(MAKE) -C emulator clean
	$(MAKE) -C assembler clean
	$(MAKE) -C llvm-backend/standalone clean
	$(MAKE) -C runtime clean
	rm -rf tests/
	rm -f *.o *.bin *.ll *.s

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