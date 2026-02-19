# SLOW-32 Self-Hosting Bootstrap Lifecycle
#
# This Makefile guides the student through the stage-by-stage progression.
# Each stage has specific prerequisites and produces clear, minimal artifacts.

.PHONY: all clean stage00 stage01 stage02

all: stage02

# Stage 00: Minimum bootstrap emulator (the trust root)
stage00:
	@echo "--- Building Stage 00: Emulator ---"
	$(MAKE) -C stage00

# Stage 01: Forth Assembler
# Prerequisites: Stage 00 emulator, Forth kernel.s32x
# Artifacts: crt0_minimal.s32o, mmio_minimal.s32o
stage01: stage00
	@echo "--- Building Stage 01: Forth Assembler Artifacts ---"
	$(MAKE) -C stage01

# Stage 02: Forth Archiver
# Prerequisites: Stage 01 artifacts
# Artifacts: libs32_minimal.s32a
stage02: stage01
	@echo "--- Building Stage 02: Forth Archiver Artifacts ---"
	$(MAKE) -C stage02

clean:
	$(MAKE) -C stage00 clean
	$(MAKE) -C stage01 clean
	$(MAKE) -C stage02 clean
