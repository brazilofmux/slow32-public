# SLOW-32 Self-Hosting Bootstrap Lifecycle
#
# This Makefile guides the student through the stage-by-stage progression.
# Each stage has specific prerequisites and produces clear, minimal artifacts.

.PHONY: all clean stage00 stage01 stage02 stage03 stage04 stage13

all: stage13

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

# Stage 03: s32-cc Compiler + Tools (as, ar, ld) + libc/runtime
# Prerequisites: Stage 02 cc-min (compiler), Stage 02 assembler + linker
# s32cc compiled by cc-min; tools compiled by s32cc. Self-sufficient toolchain.
# Artifacts: s32cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
stage03: stage02
	@echo "--- Building Stage 03: s32-cc Compiler + Tools ---"
	$(MAKE) -C stage03

# Stage 04: s12cc AST-based C Compiler + Tools (as, ar, ld)
# Prerequisites: Stage 03 (compiler, assembler, linker)
# s12cc compiled by s32cc; tools compiled by s12cc. Self-sufficient toolchain.
# Artifacts: s12cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
stage04: stage03
	@echo "--- Building Stage 04: s12cc AST Compiler + Tools ---"
	$(MAKE) -C stage04

# Stage 13: Complete s12cc toolchain (compiler + assembler + archiver + linker + runtime)
# Prerequisites: Stage 04 s12cc (compiler), Stage 04 assembler + linker
# Artifacts: s12cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
# All C compiled by stage04's s12cc. Runtime (libc) included.
stage13: stage04
	@echo "--- Building Stage 13: Complete s12cc Toolchain ---"
	$(MAKE) -C stage13

clean:
	$(MAKE) -C stage00 clean
	$(MAKE) -C stage01 clean
	$(MAKE) -C stage02 clean
	$(MAKE) -C stage03 clean
	$(MAKE) -C stage04 clean
	$(MAKE) -C stage13 clean
