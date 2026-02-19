# Stage 01: Forth Assembler + Archiver
# Produces minimal runtime object files and archive for the bootstrap process.

EMU = ../stage00/s32-emu
KERNEL = ../../forth/kernel.s32x
PRELUDE = ../../forth/prelude.fth
ASM = asm.fth
AR = ar.fth

# Artifacts
OBJECTS = crt0_minimal.s32o mmio_minimal.s32o
ARCHIVE = libs32_minimal.s32a

.PHONY: all clean

all: $(OBJECTS) $(ARCHIVE)

%.s32o: %.s $(ASM) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Assembling $< -> $@"
	@printf 'S" $<" S" $@" ASSEMBLE\nBYE\n' | cat $(PRELUDE) $(ASM) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

$(ARCHIVE): mmio_minimal.s32o $(AR) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Archiving mmio_minimal.s32o -> $@"
	@printf 'S" $@" AR-C-BEGIN S" mmio_minimal.s32o" AR-ADD AR-C-END\nBYE\n' | cat $(PRELUDE) $(AR) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

clean:
	rm -f $(OBJECTS) $(ARCHIVE)
