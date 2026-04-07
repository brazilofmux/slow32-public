# SLOW-32 Self-Hosting Bootstrap Lifecycle
#
# This Makefile guides the student through the stage-by-stage progression.
# Each stage has specific prerequisites and produces clear, minimal artifacts.

.PHONY: all clean stage00 stage01 stage02 stage03 stage04 stage05 stage06 stage07

all: stage07

# Stage 00: Minimum bootstrap emulator (the trust root)
stage00:
	@echo "--- Building Stage 00: Emulator ---"
	$(MAKE) -C stage00

# Stage 01: Forth Assembler + Archiver + Linker + C Compiler
# Prerequisites: Stage 00 emulator, Forth kernel.s32x
# Artifacts: crt0_minimal.s32o, mmio_minimal.s32o, libs32_minimal.s32a, hello_minimal.s32x, test1.s32x
stage01: stage00
	@echo "--- Building Stage 01: Forth Toolchain Artifacts ---"
	$(MAKE) -C stage01

# Stage 02: Subset-C Assembler + Archiver + Linker + Compiler
# Prerequisites: Stage 01 C compiler + Forth tools
# After this stage, ALL Forth tools (stage01) are retired.
# Artifacts: s32-as.s32x (C assembler), s32-ar.s32x (C archiver), s32-ld.s32x (C linker), cc-min.s32x (C compiler)
stage02: stage01
	@echo "--- Building Stage 02: C Assembler + Archiver + Linker + Compiler ---"
	$(MAKE) -C stage02

# Stage 03: Compiler + Tools (as, ar, ld) + libc/runtime
# Prerequisites: Stage 02 cc-min (compiler), Stage 02 assembler + linker
# compiler compiled by cc-min; tools compiled by compiler. Self-sufficient toolchain.
# Artifacts: cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
stage03: stage02
	@echo "--- Building Stage 03: Compiler + Tools ---"
	$(MAKE) -C stage03

# Stage 04: Compiler + Tools (as, ar, ld)
# Prerequisites: Stage 03 (compiler, assembler, linker)
# compiler compiled by stage03 compiler; tools compiled by compiler. Self-sufficient toolchain.
# Artifacts: cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
stage04: stage03
	@echo "--- Building Stage 04: Compiler + Tools ---"
	$(MAKE) -C stage04

# Stage 05: Code quality optimized toolchain
# Prerequisites: Stage 04 compiler, Stage 04 assembler + linker
# Artifacts: cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
# All C compiled by stage04 compiler. Runtime (libc) included.
stage05: stage04
	@echo "--- Building Stage 05: Optimized Toolchain ---"
	$(MAKE) -C stage05

# Stage 06: Next-generation selfhost toolchain
# Prerequisites: Stage 05 compiler, Stage 05 assembler + linker
# Artifacts: cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
# All C compiled by stage05 compiler. Runtime (libc) included.
stage06: stage05
	@echo "--- Building Stage 06: Selfhost Toolchain ---"
	$(MAKE) -C stage06

# Stage 07: Next-generation selfhost toolchain
# Prerequisites: Stage 06 compiler, Stage 06 assembler + linker
# Artifacts: cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
# All C compiled by stage06 compiler. Runtime (libc) included.
stage07: stage06
	@echo "--- Building Stage 07: Selfhost Toolchain ---"
	$(MAKE) -C stage07

clean:
	$(MAKE) -C stage00 clean
	$(MAKE) -C stage01 clean
	$(MAKE) -C stage02 clean
	$(MAKE) -C stage03 clean
	$(MAKE) -C stage04 clean
	$(MAKE) -C stage05 clean
	$(MAKE) -C stage06 clean
	$(MAKE) -C stage07 clean
