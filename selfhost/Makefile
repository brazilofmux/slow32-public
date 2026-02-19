# SLOW-32 Self-Hosting Bootstrap Lifecycle
#
# This Makefile guides the student through the stage-by-stage progression.
# Each stage has specific prerequisites and produces clear, minimal artifacts.

.PHONY: all clean stage00 stage01 stage02 stage03 stage04 stage05 stage06 stage07 stage08 stage09 stage10 stage11 stage12

all: stage12

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

# Stage 03: Forth Linker
# Prerequisites: Stage 01/02 artifacts
# Artifacts: hello_minimal.s32x
stage03: stage02
	@echo "--- Building Stage 03: Forth Linker Artifacts ---"
	$(MAKE) -C stage03

# Stage 04: Forth-Hosted Subset C Compiler
# Prerequisites: Stage 01/02/03 artifacts
# Artifacts: test1.s32x (compiled from C)
stage04: stage03
	@echo "--- Building Stage 04: Forth C Compiler Artifacts ---"
	$(MAKE) -C stage04

# Stage 05: Subset-C Assembler
# Prerequisites: Stage 04 C compiler, Stage 01/03 Forth tools
# Artifacts: s32-as.s32x (C assembler, runs on emulator)
stage05: stage04
	@echo "--- Building Stage 05: C Assembler ---"
	$(MAKE) -C stage05

# Stage 06: Subset-C Archiver
# Prerequisites: Stage 04 C compiler, Stage 05 C assembler, Stage 03 linker
# Artifacts: s32-ar.s32x (C archiver, runs on emulator)
stage06: stage05
	@echo "--- Building Stage 06: C Archiver ---"
	$(MAKE) -C stage06

# Stage 07: Subset-C Linker
# Prerequisites: Stage 04 C compiler, Stage 05 C assembler
# Uses Stage 03 link.fth one final time to link itself (bootstrap).
# After this stage, Forth tools (stage01-03) are no longer needed.
# Artifacts: s32-ld.s32x (C linker, runs on emulator)
stage07: stage06
	@echo "--- Building Stage 07: C Linker ---"
	$(MAKE) -C stage07

# Stage 08: Subset-C Compiler (cc-min)
# Prerequisites: Stage 04 cc.fth (one last time), Stage 05 C assembler, Stage 07 C linker
# After this stage, ALL Forth tools (stage01-04) are retired.
# Artifacts: cc-min.s32x (C compiler, runs on emulator)
stage08: stage07
	@echo "--- Building Stage 08: C Compiler ---"
	$(MAKE) -C stage08

# Stage 09: Self-Compilation Fixed-Point Proof
# Prerequisites: Stage 08 cc-min, Stage 05 C assembler, Stage 07 C linker
# cc-min compiles itself. No Forth tools. gen2.s == gen3.s proves the fixed point.
# Artifacts: cc-min.s32x (Gen2: self-compiled compiler)
stage09: stage08
	@echo "--- Building Stage 09: Self-Compiled Compiler ---"
	$(MAKE) -C stage09

# Stage 10: s32-cc Compiler (lex/parse split)
# Prerequisites: Stage 09 cc-min (compiler), Stage 05 assembler, Stage 07 linker
# Artifacts: s32cc.s32x (s32-cc compiler, runs on emulator)
stage10: stage09
	@echo "--- Building Stage 10: s32-cc Compiler ---"
	$(MAKE) -C stage10

# Stage 11: s32-cc Compiler (stable bootstrap, copy of stage10)
# Prerequisites: Stage 10 s32cc (compiler), Stage 05 assembler, Stage 07 linker
# Artifacts: s32cc.s32x (s32-cc compiler with nested switch fix + long branches)
stage11: stage10
	@echo "--- Building Stage 11: s32-cc Compiler (stable) ---"
	$(MAKE) -C stage11

# Stage 12: s12cc AST-based C Compiler
# Prerequisites: Stage 11 s32cc (compiler), Stage 05 assembler, Stage 07 linker
# Artifacts: s12cc.s32x (AST-based compiler, Ragel lexer + recursive-descent parser)
stage12: stage11
	@echo "--- Building Stage 12: s12cc AST Compiler ---"
	$(MAKE) -C stage12

clean:
	$(MAKE) -C stage00 clean
	$(MAKE) -C stage01 clean
	$(MAKE) -C stage02 clean
	$(MAKE) -C stage03 clean
	$(MAKE) -C stage04 clean
	$(MAKE) -C stage05 clean
	$(MAKE) -C stage06 clean
	$(MAKE) -C stage07 clean
	$(MAKE) -C stage08 clean
	$(MAKE) -C stage09 clean
	$(MAKE) -C stage10 clean
	$(MAKE) -C stage11 clean
	$(MAKE) -C stage12 clean
