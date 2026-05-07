# stage07-cross Makefile — Cross-compiler: C → x86-64 ELF
#
# Bootstrap: cc-x64 is compiled directly by host GCC against host libc
# (mirrors the cc-a64 build). cc-x64 then emits crt0.o and compiles
# libc_x64/*.c into libc_x64.a, which are used to link self-hosted
# outputs (s32fast-hir, dbt-x64, tests).
#
# Usage:
#   make              — build cc-x64 and s32fast-hir
#   make test         — compile and run all tests
#   make bench        — run benchmark_core through s32fast-hir
#   make clean        — remove generated .o files and binaries

CC = gcc
CFLAGS = -O2 -fno-builtin -fno-stack-protector -w
LDFLAGS = -nostdlib -static
OUT = out

# Source headers that trigger cc-x64 recompilation
CC_DEPS = cc-x64.c hir_regalloc_x64.h hir_codegen_x64.h hir_burg_x64.h \
          x64_encode.h elf_writer.h obj_writer.h codegen_x64.h libc_x64.h \
          ../stage07/hir.h ../stage07/hir_lower.h ../stage07/hir_ssa.h \
          ../stage07/hir_opt.h ../stage07/hir_licm.h ../stage07/hir_burg.h \
          ../stage07/parser.h ../stage07/ast.h ../stage07/sema.h \
          ../stage07/optimize.h ../stage07/pp.h c_lexer_gen.c

.PHONY: all test bench clean libc

all: $(OUT)/cc-x64 $(OUT)/s32fast-hir

$(OUT):
	mkdir -p $(OUT)

# --- Compiler / linker / archiver: host-GCC-built, self-contained ---
# Built directly against host libc (no chicken-and-egg with crt0/libc_x64.a).

$(OUT)/cc-x64: $(CC_DEPS) | $(OUT)
	$(CC) $(CFLAGS) -o $@ cc-x64.c

$(OUT)/ld-x64: ld-x64.c x64_encode.h elf_writer.h | $(OUT)
	gcc -O2 -Wall -o $@ ld-x64.c

$(OUT)/ar-x64: ar-x64.c | $(OUT)
	gcc -O2 -Wall -o $@ ar-x64.c

# --- crt0 (emitted by cc-x64) ---

$(OUT)/crt0.o: $(OUT)/cc-x64
	$(OUT)/cc-x64 --crt0 -o $@

# --- libc_x64.a ---
# Most libc objects compile through cc-x64 --hir.  fpu_ops.c is host-gcc-
# built because it uses SSE intrinsics that cc-x64 cannot yet emit (see
# the comment at the top of libc_x64/fpu_ops.c).

LIBC_X64_HIR_SRCS = libc_x64/syscalls.c libc_x64/string.c libc_x64/stdio.c \
                    libc_x64/malloc.c libc_x64/misc.c libc_x64/mmio_ring_x64.c
LIBC_X64_HIR_OBJS = $(patsubst libc_x64/%.c,$(OUT)/libc_x64_%.o,$(LIBC_X64_HIR_SRCS))

$(OUT)/libc_x64_%.o: libc_x64/%.c $(OUT)/cc-x64
	$(OUT)/cc-x64 --hir -c $< -o $@

# fpu_ops.o, math_stubs.o — host-gcc-built (use <math.h> / __builtin_sqrt)
$(OUT)/libc_x64_fpu_ops.o: libc_x64/fpu_ops.c | $(OUT)
	gcc -O2 -fno-builtin -fno-stack-protector -w -c $< -o $@

$(OUT)/libc_x64_math_stubs.o: libc_x64/math_stubs.c | $(OUT)
	gcc -O2 -fno-builtin -fno-stack-protector -w -c $< -o $@

LIBC_X64_HOST_OBJS = $(OUT)/libc_x64_fpu_ops.o $(OUT)/libc_x64_math_stubs.o

$(OUT)/libc_x64.a: $(LIBC_X64_HIR_OBJS) $(LIBC_X64_HOST_OBJS)
	ar rcs $@ $(LIBC_X64_HIR_OBJS) $(LIBC_X64_HOST_OBJS)

libc: $(OUT)/libc_x64.a $(OUT)/crt0.o

# --- Emulator (s32fast-hir) ---

$(OUT)/s32fast-hir.o: s32-fast-x64.c $(OUT)/cc-x64
	$(OUT)/cc-x64 --hir -c $< -o $@

$(OUT)/s32fast-hir: $(OUT)/s32fast-hir.o $(OUT)/crt0.o $(OUT)/libc_x64.a
	$(CC) $(LDFLAGS) -o $@ $(OUT)/crt0.o $< $(OUT)/libc_x64.a

# --- dbt-x64 — SLOW-32 dynamic binary translator, compiled by cc-x64 --hir ---
# x86-64-relevant subset of tools/dbt/.  emit_a64.c and translate_a64.c are
# excluded (those are the AArch64 backend).  The MMIO ring is provided by
# libc_x64/mmio_ring_x64.c (a focused stub of tools/emulator/mmio_ring.c).

DBT_DIR = ../../tools/dbt
DBT_SRCS = $(DBT_DIR)/dbt.c $(DBT_DIR)/emit_x64.c $(DBT_DIR)/translate.c \
           $(DBT_DIR)/block_cache.c $(DBT_DIR)/shadow_interp.c \
           $(DBT_DIR)/stage5_lift.c $(DBT_DIR)/stage5_burg.c \
           $(DBT_DIR)/stage5_ssa.c $(DBT_DIR)/stage5_ra.c \
           $(DBT_DIR)/stage5_mir.c $(DBT_DIR)/stage5_lir_opt.c \
           $(DBT_DIR)/stage5_codegen.c
DBT_OBJS = $(patsubst $(DBT_DIR)/%.c,$(OUT)/dbt_%.o,$(DBT_SRCS))

$(OUT)/dbt_%.o: $(DBT_DIR)/%.c $(OUT)/cc-x64
	$(OUT)/cc-x64 --hir -c $< -o $@

$(OUT)/dbt-x64: $(DBT_OBJS) $(OUT)/crt0.o $(OUT)/libc_x64.a
	$(CC) $(LDFLAGS) -o $@ $(OUT)/crt0.o $(DBT_OBJS) $(OUT)/libc_x64.a

dbt: $(OUT)/dbt-x64

.PHONY: dbt

# --- Tests ---

# Skip tests that don't compile with the self-hosted compiler
TEST_SKIP = tests/test_libc.c tests/test_x0_x1.c
TEST_SRCS = $(filter-out $(TEST_SKIP),$(wildcard tests/test_*.c))
TEST_BINS = $(patsubst tests/%.c,$(OUT)/%,$(TEST_SRCS))

$(OUT)/test_%: tests/test_%.c $(OUT)/cc-x64 $(OUT)/crt0.o $(OUT)/libc_x64.a
	$(OUT)/cc-x64 --hir -I . -c $< -o $(OUT)/$*.o
	$(CC) $(LDFLAGS) -o $@ $(OUT)/crt0.o $(OUT)/$*.o $(OUT)/libc_x64.a

test: $(TEST_BINS)
	@echo "--- Running tests ---"
	@for t in $(TEST_BINS); do \
		name=$$(basename $$t); \
		$$t >/dev/null 2>&1; rc=$$?; \
		echo "  $$name: exit=$$rc"; \
	done

# --- Benchmark ---

BENCH_S32X ?= $(HOME)/slow-32/benchmark_core.s32x

bench: $(OUT)/s32fast-hir
	@if [ ! -f "$(BENCH_S32X)" ]; then \
		echo "Benchmark not found: $(BENCH_S32X)"; \
		echo "Set BENCH_S32X=path/to/benchmark_core.s32x"; \
		exit 1; \
	fi
	time $(OUT)/s32fast-hir $(BENCH_S32X)

# --- Clean ---

clean:
	rm -f $(OUT)/cc-x64 $(OUT)/ld-x64 $(OUT)/ld-x64.o $(OUT)/ar-x64
	rm -f $(OUT)/crt0.o $(OUT)/libc_x64.a $(LIBC_X64_HIR_OBJS)
	rm -f $(OUT)/s32fast-hir.o $(OUT)/s32fast-hir
	rm -f $(patsubst tests/%.c,$(OUT)/%.o,$(TEST_SRCS)) $(TEST_BINS)
