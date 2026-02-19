# Stage 04: Forth-Hosted C Compiler
# Compiles Subset C to SLOW-32 assembly.

EMU = ../stage00/s32-emu
KERNEL = ../../forth/kernel.s32x
PRELUDE = ../../forth/prelude.fth
CC = cc.fth
ASM = ../stage01/asm.fth
LINK = ../stage03/link.fth

# Prerequisites from Stage 01
STAGE01_DIR = ../stage01
CRT0 = $(STAGE01_DIR)/crt0_minimal.s32o
LIBS32 = $(STAGE01_DIR)/libs32_minimal.s32a

# Test program
TEST_SRC = tests/test1.c
TEST_ASM = test1.s
TEST_OBJ = test1.s32o
TEST_EXE = test1.s32x

.PHONY: all clean test

all: $(TEST_EXE)

$(TEST_ASM): $(TEST_SRC) $(CC) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Compiling $< -> $@"
	@printf 'S" $<" S" $@" COMPILE-FILE\nBYE\n' | cat $(PRELUDE) $(CC) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

$(TEST_OBJ): $(TEST_ASM) $(ASM) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Assembling $< -> $@"
	@printf 'S" $<" S" $@" ASSEMBLE\nBYE\n' | cat $(PRELUDE) $(ASM) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

$(TEST_EXE): $(TEST_OBJ) $(CRT0) $(LIBS32) $(LINK) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Linking $@..."
	@printf 'LINK-INIT S" $(CRT0)" LINK-OBJ S" $(TEST_OBJ)" LINK-OBJ S" $(LIBS32)" LINK-ARCHIVE S" $@" LINK-EMIT\nBYE\n' | cat $(PRELUDE) $(LINK) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

test: $(TEST_EXE)
	@echo "--- Running compiled C test program ---"
	@$(EMU) $(TEST_EXE)

clean:
	rm -f $(TEST_ASM) $(TEST_OBJ) $(TEST_EXE)
