# Stage 02: Forth Archiver
# Produces libs32_minimal.s32a containing mmio_minimal.s32o.

EMU = ../stage00/s32-emu
KERNEL = ../../forth/kernel.s32x
PRELUDE = ../../forth/prelude.fth
AR = ar.fth

# Prerequisites from Stage 01
STAGE01_DIR = ../stage01
MMIO_OBJ = $(STAGE01_DIR)/mmio_minimal.s32o

# Artifacts
ARCHIVE = libs32_minimal.s32a

.PHONY: all clean

all: $(ARCHIVE)

$(ARCHIVE): $(MMIO_OBJ) $(AR) $(EMU) $(KERNEL) $(PRELUDE)
	@echo "Archiving $(MMIO_OBJ) -> $@"
	@printf 'S" $@" AR-C-BEGIN S" $(MMIO_OBJ)" AR-ADD AR-C-END\nBYE\n' | cat $(PRELUDE) $(AR) - | $(EMU) $(KERNEL) > /dev/null || [ $$? -eq 96 ]

clean:
	rm -f $(ARCHIVE)
