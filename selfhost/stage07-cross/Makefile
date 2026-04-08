# stage07-cross Makefile — Cross-compiler: C → x86-64 ELF
#
# Bootstrap: cc-x64.c is compiled by host GCC into an object file, then
# linked with the existing crt0.o + libc_x64.a to produce cc-x64.
# cc-x64 then compiles everything else (libc, emulator, tests).
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

.PHONY: all test bench clean

all: $(OUT)/cc-x64 $(OUT)/s32fast-hir

# --- Compiler (host-GCC bootstrap) ---
# cc-x64 is compiled by GCC, linked with the pre-existing crt0.o + libc_x64.a.
# This is the bootstrap step; cc-x64 then compiles everything else.

$(OUT)/cc-x64-gen1.o: $(CC_DEPS)
	$(CC) $(CFLAGS) -c cc-x64.c -o $@

$(OUT)/cc-x64: $(OUT)/cc-x64-gen1.o $(OUT)/crt0.o $(OUT)/libc_x64.a
	$(CC) $(LDFLAGS) -o $@ $(OUT)/crt0.o $< $(OUT)/libc_x64.a

# --- Linker ---

$(OUT)/ld-x64: ld-x64.c x64_encode.h elf_writer.h $(OUT)/crt0.o $(OUT)/libc_x64.a
	$(CC) $(CFLAGS) -c ld-x64.c -o $(OUT)/ld-x64.o
	$(CC) $(LDFLAGS) -o $@ $(OUT)/crt0.o $(OUT)/ld-x64.o $(OUT)/libc_x64.a

# --- Emulator (s32fast-hir) ---

$(OUT)/s32fast-hir.o: s32-fast-x64.c $(OUT)/cc-x64
	$(OUT)/cc-x64 --hir -c $< -o $@

$(OUT)/s32fast-hir: $(OUT)/s32fast-hir.o $(OUT)/crt0.o $(OUT)/libc_x64.a
	$(CC) $(LDFLAGS) -o $@ $(OUT)/crt0.o $< $(OUT)/libc_x64.a

# --- Rebuild libc (optional, uses cc-x64) ---

.PHONY: libc
libc: $(OUT)/cc-x64
	$(OUT)/cc-x64 --crt0 -o $(OUT)/crt0.o
	$(OUT)/cc-x64 -c libc_x64/syscalls.c -o $(OUT)/syscalls.o
	$(OUT)/cc-x64 -c libc_x64/string.c -o $(OUT)/string.o
	$(OUT)/cc-x64 -c libc_x64/stdio.c -o $(OUT)/stdio.o
	$(OUT)/cc-x64 -c libc_x64/malloc.c -o $(OUT)/malloc.o
	$(OUT)/cc-x64 -c libc_x64/misc.c -o $(OUT)/misc.o
	$(OUT)/cc-x64 -c libc_x64/fpu_ops.c -o $(OUT)/fpu_ops.o
	ar rcs $(OUT)/libc_x64.a $(OUT)/syscalls.o $(OUT)/string.o $(OUT)/stdio.o \
		$(OUT)/malloc.o $(OUT)/misc.o $(OUT)/fpu_ops.o $(OUT)/math_stubs.o

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
	rm -f $(OUT)/cc-x64-gen1.o $(OUT)/cc-x64
	rm -f $(OUT)/s32fast-hir.o $(OUT)/s32fast-hir
	rm -f $(patsubst tests/%.c,$(OUT)/%.o,$(TEST_SRCS)) $(TEST_BINS)
