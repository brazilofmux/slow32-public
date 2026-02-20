# SLOW-32 Self-Hosting Bootstrap Lifecycle
#
# This Makefile guides the student through the stage-by-stage progression.
# Each stage has specific prerequisites and produces clear, minimal artifacts.

.PHONY: all clean stage00 stage01 stage02 stage09 stage10 stage11 stage12 stage13 stage14

all: stage14

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

# Stage 09: Self-Compilation Fixed-Point Proof
# Prerequisites: Stage 02 cc-min, Stage 02 C assembler + linker
# cc-min compiles itself. No Forth tools. gen2.s == gen3.s proves the fixed point.
# Artifacts: cc-min.s32x (Gen2: self-compiled compiler)
stage09: stage02
	@echo "--- Building Stage 09: Self-Compiled Compiler ---"
	$(MAKE) -C stage09

# Stage 10: s32-cc Compiler (lex/parse split)
# Prerequisites: Stage 09 cc-min (compiler), Stage 02 assembler + linker
# Artifacts: s32cc.s32x (s32-cc compiler, runs on emulator)
stage10: stage09
	@echo "--- Building Stage 10: s32-cc Compiler ---"
	$(MAKE) -C stage10

# Stage 11: s32-cc Compiler (stable bootstrap, copy of stage10)
# Prerequisites: Stage 10 s32cc (compiler), Stage 02 assembler + linker
# Artifacts: s32cc.s32x (s32-cc compiler with nested switch fix + long branches)
stage11: stage10
	@echo "--- Building Stage 11: s32-cc Compiler (stable) ---"
	$(MAKE) -C stage11

# Stage 12: s12cc AST-based C Compiler
# Prerequisites: Stage 11 s32cc (compiler), Stage 02 assembler + linker
# Artifacts: s12cc.s32x (AST-based compiler, Ragel lexer + recursive-descent parser)
stage12: stage11
	@echo "--- Building Stage 12: s12cc AST Compiler ---"
	$(MAKE) -C stage12

# Stage 13: Complete s12cc toolchain (compiler + assembler + archiver + linker + runtime)
# Prerequisites: Stage 12 s12cc (compiler), Stage 02 assembler + linker
# Artifacts: s12cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
# All C compiled by stage12's s12cc. Runtime (libc) included.
stage13: stage12
	@echo "--- Building Stage 13: Complete s12cc Toolchain ---"
	$(MAKE) -C stage13

# Stage 14: Compiler playground (copy of stage13, bootstrapped by stage13)
# Prerequisites: Stage 13 s12cc (compiler), Stage 02 assembler + linker
# Artifacts: s12cc.s32x, s32-as.s32x, s32-ar.s32x, s32-ld.s32x
stage14: stage13
	@echo "--- Building Stage 14: s12cc playground ---"
	$(MAKE) -C stage14

clean:
	$(MAKE) -C stage00 clean
	$(MAKE) -C stage01 clean
	$(MAKE) -C stage02 clean
	$(MAKE) -C stage09 clean
	$(MAKE) -C stage10 clean
	$(MAKE) -C stage11 clean
	$(MAKE) -C stage12 clean
	$(MAKE) -C stage13 clean
	$(MAKE) -C stage14 clean
