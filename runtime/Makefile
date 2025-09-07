# SLOW-32 Runtime Library Makefile

# Paths
LLVM_BIN = ~/llvm-project/build/bin
ASSEMBLER = ../assembler/slow32asm

# Compiler flags
CLANG = $(LLVM_BIN)/clang
LLC = $(LLVM_BIN)/llc
TARGET = slow32-unknown-none
CFLAGS = -target $(TARGET) -S -emit-llvm -O2

# Object files
RUNTIME_OBJS = crt0.s32o intrinsics.s32o intrinsics_extra.s32o \
               stdlib.s32o putchar.s32o debug_char.s32o \
               printf.s32o builtins.s32o

# Default target
all: $(RUNTIME_OBJS)

# Pattern rule for .s -> .s32o
%.s32o: %.s
	$(ASSEMBLER) $< $@

# Special rule for builtins (C -> LLVM IR -> ASM -> OBJ)
builtins.ll: builtins.c
	$(CLANG) $(CFLAGS) $< -o $@

builtins.s: builtins.ll
	$(LLC) -mtriple=$(TARGET) $< -o $@

# Special rule for printf (C -> LLVM IR -> ASM -> OBJ)
printf.ll: printf.c
	$(CLANG) $(CFLAGS) $< -o $@

printf.s: printf.ll  
	$(LLC) -mtriple=$(TARGET) $< -o $@

# Clean target
clean:
	rm -f *.s32o *.ll

# Install target (for future use)
install: all
	@echo "Runtime objects built successfully"

.PHONY: all clean install