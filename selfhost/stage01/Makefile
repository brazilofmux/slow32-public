# Stage 01: Forth Assembler
# Produces minimal runtime object files for the bootstrap process.

EMU = ../stage00/s32-emu
KERNEL = ../../forth/kernel.s32x
PRELUDE = ../../forth/prelude.fth
ASM = asm.fth

# Artifacts
OBJECTS = crt0_minimal.s32o mmio_minimal.s32o

.PHONY: all clean

all: $(OBJECTS)

%.s32o: %.s $(ASM) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Assembling $< -> $@"
	@printf 'S" $<" S" $@" ASSEMBLE\nBYE\n' | cat $(PRELUDE) $(ASM) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

clean:
	rm -f $(OBJECTS)
