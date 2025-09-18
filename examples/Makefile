# Makefile for SLOW-32 examples
# Adjust paths if your setup differs

SLOW32 = ..
LLVM = ~/llvm-project/build/bin

# Tools
CC = $(LLVM)/clang
LLC = $(LLVM)/llc
AS = $(SLOW32)/tools/assembler/slow32asm
LD = $(SLOW32)/tools/linker/s32-ld
EMU = $(SLOW32)/tools/emulator/slow32
RUNTIME = $(SLOW32)/runtime

# Flags
TARGET = slow32-unknown-none
CFLAGS = --target=$(TARGET) -S -emit-llvm -O1
LLCFLAGS = -mtriple=$(TARGET)

# Example programs
ASM_EXAMPLES = hello-debug fibonacci
C_EXAMPLES = hello factorial arrays

ALL_EXAMPLES = $(ASM_EXAMPLES) $(C_EXAMPLES)
ALL_EXECUTABLES = $(addsuffix .s32x, $(ALL_EXAMPLES))

# Default target
all: $(ALL_EXECUTABLES)

# Assembly examples (direct assembly)
hello-debug.s32o: hello-debug.s
	$(AS) -o $@ $<

fibonacci.s32o: fibonacci.s
	$(AS) -o $@ $<

# Pattern rules for C compilation
%.ll: %.c
	$(CC) $(CFLAGS) $< -o $@

%.s: %.ll
	$(LLC) $(LLCFLAGS) $< -o $@

%.s32o: %.s
	$(AS) -o $@ $<

# Linking rules
hello-debug.s32x: hello-debug.s32o
	$(LD) -o $@ $(RUNTIME)/crt0.s32o $<

fibonacci.s32x: fibonacci.s32o
	$(LD) -o $@ $(RUNTIME)/crt0.s32o $<

%.s32x: %.s32o
	$(LD) -o $@ $(RUNTIME)/crt0.s32o $< $(RUNTIME)/libs32.s32a $(RUNTIME)/libc_debug.s32a

# Run targets
run-%: %.s32x
	@echo "Running $<..."
	@$(EMU) $<

# Debug targets (with trace)
debug-%: %.s32x
	@echo "Debug trace for $<..."
	$(EMU) -t $< | head -50

# Test all examples
test: $(ALL_EXECUTABLES)
	@echo "Testing all examples..."
	@for exe in $(ALL_EXECUTABLES); do \
		echo "Running $$exe..."; \
		$(EMU) -c 10000 $$exe || true; \
		echo "---"; \
	done

# Clean build artifacts
clean:
	rm -f *.ll *.s *.s32o *.s32x

# Help target
help:
	@echo "SLOW-32 Examples Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all          - Build all examples"
	@echo "  run-NAME     - Run specific example (e.g., run-hello)"
	@echo "  debug-NAME   - Debug trace for example"
	@echo "  test         - Test all examples"
	@echo "  clean        - Remove build artifacts"
	@echo ""
	@echo "Examples:"
	@echo "  Assembly: $(ASM_EXAMPLES)"
	@echo "  C:        $(C_EXAMPLES)"

.PHONY: all test clean help