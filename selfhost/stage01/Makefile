# Stage 01: Forth Assembler + Archiver + Linker + C Compiler
# Produces minimal runtime object files, archive, linked test executable,
# and a C-compiled test program.

EMU = ../stage00/s32-emu
KERNEL = ../../forth/kernel.s32x
PRELUDE = ../../forth/prelude.fth
ASM = asm.fth
AR = ar.fth
LINK = link.fth
CC = cc.fth

# Artifacts
OBJECTS = crt0_minimal.s32o mmio_minimal.s32o
ARCHIVE = libs32_minimal.s32a
HELLO_OBJ = hello_minimal.s32o
HELLO_EXE = hello_minimal.s32x

# C compiler test artifacts
TEST_SRC = tests/test1.c
TEST_ASM = test1.s
TEST_OBJ = test1.s32o
TEST_EXE = test1.s32x

.PHONY: all clean test verify

all: $(OBJECTS) $(ARCHIVE) $(HELLO_EXE) $(TEST_EXE)

%.s32o: %.s $(ASM) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Assembling $< -> $@"
	@printf 'S" $<" S" $@" ASSEMBLE\nBYE\n' | cat $(PRELUDE) $(ASM) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

$(ARCHIVE): mmio_minimal.s32o $(AR) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Archiving mmio_minimal.s32o -> $@"
	@printf 'S" $@" AR-C-BEGIN S" mmio_minimal.s32o" AR-ADD AR-C-END\nBYE\n' | cat $(PRELUDE) $(AR) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

$(HELLO_EXE): $(HELLO_OBJ) crt0_minimal.s32o $(ARCHIVE) $(LINK) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Linking $@..."
	@printf 'LINK-INIT S" crt0_minimal.s32o" LINK-OBJ S" $(HELLO_OBJ)" LINK-OBJ S" $(ARCHIVE)" LINK-ARCHIVE S" $@" LINK-EMIT\nBYE\n' | cat $(PRELUDE) $(LINK) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

# C compiler: compile test1.c -> test1.s -> test1.s32o -> test1.s32x
$(TEST_ASM): $(TEST_SRC) $(CC) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Compiling $< -> $@"
	@printf 'S" $<" S" $@" COMPILE-FILE\nBYE\n' | cat $(PRELUDE) $(CC) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

$(TEST_EXE): $(TEST_OBJ) crt0_minimal.s32o $(ARCHIVE) $(LINK) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Linking $@..."
	@printf 'LINK-INIT S" crt0_minimal.s32o" LINK-OBJ S" $(TEST_OBJ)" LINK-OBJ S" $(ARCHIVE)" LINK-ARCHIVE S" $@" LINK-EMIT\nBYE\n' | cat $(PRELUDE) $(LINK) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

test: $(HELLO_EXE) $(TEST_EXE)
	@echo "--- Running linked test program ---"
	@$(EMU) $(HELLO_EXE)
	@echo "--- Running C-compiled test program ---"
	@$(EMU) $(TEST_EXE)

verify:
	@echo "--- Fixed-point kernel verification ---"
	@bash run-selfhost-kernel.sh

clean:
	rm -f $(OBJECTS) $(ARCHIVE) $(HELLO_OBJ) $(HELLO_EXE) $(TEST_ASM) $(TEST_OBJ) $(TEST_EXE)
