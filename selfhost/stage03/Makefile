# Stage 03: Forth Linker
# Produces a runnable .s32x executable.

EMU = ../stage00/s32-emu
KERNEL = ../../forth/kernel.s32x
PRELUDE = ../../forth/prelude.fth
LINK = link.fth

# Prerequisites from previous stages
STAGE01_DIR = ../stage01
STAGE02_DIR = ../stage02
CRT0 = $(STAGE01_DIR)/crt0_minimal.s32o
LIBS32 = $(STAGE02_DIR)/libs32_minimal.s32a

# Local test program
HELLO_SRC = hello_minimal.s
HELLO_OBJ = hello_minimal.s32o
HELLO_EXE = hello_minimal.s32x

.PHONY: all clean test verify

all: $(HELLO_EXE)

# We need the stage01 assembler to build our test object
$(HELLO_OBJ): $(HELLO_SRC)
	@echo "Assembling $< -> $@"
	@printf 'S" $<" S" $@" ASSEMBLE\nBYE\n' | cat $(PRELUDE) ../stage01/asm.fth - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

$(HELLO_EXE): $(HELLO_OBJ) $(CRT0) $(LIBS32) $(LINK) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Linking $@..."
	@printf 'LINK-INIT S" $(CRT0)" LINK-OBJ S" $(HELLO_OBJ)" LINK-OBJ S" $(LIBS32)" LINK-ARCHIVE S" $@" LINK-EMIT\nBYE\n' | cat $(PRELUDE) $(LINK) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

test: $(HELLO_EXE)
	@echo "--- Running linked test program ---"
	@$(EMU) $(HELLO_EXE)

verify:
	@echo "--- Fixed-point kernel verification ---"
	@bash run-selfhost-kernel.sh

clean:
	rm -f $(HELLO_OBJ) $(HELLO_EXE)
