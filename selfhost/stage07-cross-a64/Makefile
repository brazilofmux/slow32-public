# stage07-cross-a64 Makefile — Cross-compiler: C → AArch64 ELF
#
# This is the AArch64 sibling of stage07-cross. The frontend (lex/parse/
# sema/HIR/optimize) is shared via symlinks into ../stage07; only the
# backend (encoder, codegen, BURG, regalloc, writers) and the libc/crt0
# differ from the x86-64 cross-compiler.

CC = gcc
CFLAGS = -O2 -fno-builtin -fno-stack-protector -w
LDFLAGS = -nostdlib -static
OUT = out

.PHONY: all test test-encode test-elf test-obj test-crt0 test-cc test-toolchain test-archive libc s32fast bench clean

all: $(OUT)/cc-a64 $(OUT)/ld-a64 $(OUT)/ar-a64 \
     test-encode test-elf test-obj test-crt0 test-cc test-toolchain test-archive

# --- The cross-compiler itself ---
# Built by host gcc against the shared frontend (symlinked from ../stage07).

CC_DEPS = cc-a64.c codegen_a64.h crt0_emit.h elf_writer.h obj_writer.h \
          a64_encode.h a64_reloc_kinds.h \
          hir_codegen_a64.h hir_burg_a64.h hir_regalloc_a64.h \
          ../stage07/parser.h ../stage07/ast.h ../stage07/sema.h \
          ../stage07/optimize.h ../stage07/pp.h ../stage07/c_lexer_gen.c \
          ../stage07/hir.h ../stage07/hir_lower.h ../stage07/hir_ssa.h \
          ../stage07/hir_opt.h ../stage07/hir_licm.h

$(OUT)/cc-a64: $(CC_DEPS)
	$(CC) $(CFLAGS) -o $@ cc-a64.c

# --- Linker (ld-a64) and archiver (ar-a64) ---
# Both built with host gcc against host stdio/string. Self-contained.

$(OUT)/ld-a64: ld-a64.c
	gcc -O2 -Wall -o $@ ld-a64.c

$(OUT)/ar-a64: ar-a64.c
	gcc -O2 -Wall -o $@ ar-a64.c

# --- Encoder verification (byte-diff against gas) ---

$(OUT)/test_encode: tests/test_encode.c a64_encode.h tests/ref.bin
	gcc -O2 -Wall -Wno-unused-function -o $@ tests/test_encode.c

tests/ref.bin: tests/ref.s
	tests/gen_ref.sh

test-encode: $(OUT)/test_encode
	@$(OUT)/test_encode

# --- ELF executable writer smoke test (3-inst exit(42) program) ---

$(OUT)/test_elf: tests/test_elf.c a64_encode.h elf_writer.h
	gcc -O2 -Wall -Wno-unused-function -Wno-pointer-sign -o $@ tests/test_elf.c

test-elf: $(OUT)/test_elf
	@$(OUT)/test_elf >/dev/null
	@$(OUT)/test_exit42; rc=$$?; \
		if [ $$rc -eq 42 ]; then echo "test-elf: ok (exit=42)"; \
		else echo "test-elf: FAIL (exit=$$rc, expected 42)"; exit 1; fi

# --- Relocatable .o writer smoke tests (linked with system ld) ---

$(OUT)/test_obj: tests/test_obj.c a64_encode.h obj_writer.h
	gcc -O2 -Wall -Wno-unused-function -Wno-pointer-sign -o $@ tests/test_obj.c

$(OUT)/test_obj_global: tests/test_obj_global.c a64_encode.h obj_writer.h
	gcc -O2 -Wall -Wno-unused-function -Wno-pointer-sign -o $@ tests/test_obj_global.c

test-obj: $(OUT)/test_obj $(OUT)/test_obj_global
	@$(OUT)/test_obj >/dev/null
	@ld -o $(OUT)/test_obj_exe $(OUT)/test_obj.o
	@$(OUT)/test_obj_exe; rc=$$?; \
		if [ $$rc -eq 42 ]; then echo "test-obj: ok (exit=42)"; \
		else echo "test-obj: FAIL (exit=$$rc, expected 42)"; exit 1; fi
	@$(OUT)/test_obj_global >/dev/null
	@ld -o $(OUT)/test_obj_global_exe $(OUT)/test_obj_global.o
	@$(OUT)/test_obj_global_exe; rc=$$?; \
		if [ $$rc -eq 42 ]; then echo "test-obj-global: ok (exit=42)"; \
		else echo "test-obj-global: FAIL (exit=$$rc, expected 42)"; exit 1; fi

# --- crt0 emitter smoke test ---
# Builds out/crt0.o via emit_crt0_object, links it with a host-gcc-compiled
# main (returns argc) and a stub __save_envp, then runs with various arg
# counts and verifies each exit code matches argc.

$(OUT)/test_crt0: tests/test_crt0.c a64_encode.h obj_writer.h crt0_emit.h tests/cg_stubs.h
	gcc -O2 -Wall -Wno-unused-function -Wno-pointer-sign -o $@ tests/test_crt0.c

$(OUT)/crt0_main.o: tests/crt0_main.c
	gcc -O2 -nostdlib -fno-stack-protector -ffreestanding -c $< -o $@

$(OUT)/crt0_stubs.o: tests/crt0_stubs.c
	gcc -O2 -nostdlib -fno-stack-protector -ffreestanding -c $< -o $@

test-crt0: $(OUT)/test_crt0 $(OUT)/crt0_main.o $(OUT)/crt0_stubs.o
	@$(OUT)/test_crt0 >/dev/null
	@ld -o $(OUT)/test_crt0_prog $(OUT)/crt0.o $(OUT)/crt0_main.o $(OUT)/crt0_stubs.o
	@$(OUT)/test_crt0_prog; rc=$$?; \
		if [ $$rc -eq 1 ]; then echo "test-crt0 (no args):   ok (exit=1)"; \
		else echo "test-crt0 (no args):   FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/test_crt0_prog one two; rc=$$?; \
		if [ $$rc -eq 3 ]; then echo "test-crt0 (3 args):    ok (exit=3)"; \
		else echo "test-crt0 (3 args):    FAIL (exit=$$rc, expected 3)"; exit 1; fi
	@$(OUT)/test_crt0_prog a b c d e f g; rc=$$?; \
		if [ $$rc -eq 8 ]; then echo "test-crt0 (8 args):    ok (exit=8)"; \
		else echo "test-crt0 (8 args):    FAIL (exit=$$rc, expected 8)"; exit 1; fi

# --- End-to-end cc-a64 smoke tests ---
# Each entry: compile via cc-a64, link with crt0.o + crt0_stubs.o, run,
# verify exit code.

# Compile a single .c under tests/ via cc-a64.
$(OUT)/cc_%.o: tests/cc_%.c $(OUT)/cc-a64
	@$(OUT)/cc-a64 -c $< -o $@ >/dev/null

$(OUT)/cc_multi_include.o: tests/cc_multi_include.c $(OUT)/cc-a64 \
                           tests/inc_one/one.h tests/inc_two/two.h
	@$(OUT)/cc-a64 -Itests/inc_one -Itests/inc_two -c $< -o $@ >/dev/null

$(OUT)/cc_%_exe: $(OUT)/cc_%.o $(OUT)/crt0.o $(OUT)/crt0_stubs.o
	@ld -o $@ $(OUT)/crt0.o $< $(OUT)/crt0_stubs.o

test-cc: $(OUT)/cc_hello_exe $(OUT)/cc_locals_exe \
         $(OUT)/cc_loop_exe  $(OUT)/cc_recursion_exe \
         $(OUT)/cc_phi_swap_exe $(OUT)/cc_fwd_struct_exe \
         $(OUT)/cc_typedef_enum_exe $(OUT)/cc_proto_unnamed_exe \
         $(OUT)/cc_bool_exe $(OUT)/cc_ifdef_aarch64_exe \
         $(OUT)/cc_size_t_exe $(OUT)/cc_adj_string_exe \
         $(OUT)/cc_struct_multi_decl_exe $(OUT)/cc_global_struct_table_exe \
         $(OUT)/cc_local_static_assert_exe $(OUT)/cc_fwd_struct_offsetof_exe \
         $(OUT)/cc_macro_paren_obj_exe $(OUT)/cc_anon_nested_struct_exe \
         $(OUT)/cc_multi_include_exe \
         $(OUT)/cc_inline_asm_a64_exe \
         $(OUT)/cc_string_exe
	@$(OUT)/cc_hello_exe;     rc=$$?; if [ $$rc -eq 42 ];  then echo "cc_hello:     ok (exit=42)";     else echo "cc_hello:     FAIL (exit=$$rc, expected 42)";     exit 1; fi
	@$(OUT)/cc_locals_exe;    rc=$$?; if [ $$rc -eq 42 ];  then echo "cc_locals:    ok (exit=42)";     else echo "cc_locals:    FAIL (exit=$$rc, expected 42)";     exit 1; fi
	@$(OUT)/cc_loop_exe;      rc=$$?; if [ $$rc -eq 47 ];  then echo "cc_loop:      ok (exit=47)";     else echo "cc_loop:      FAIL (exit=$$rc, expected 47)";     exit 1; fi
	@$(OUT)/cc_recursion_exe; rc=$$?; if [ $$rc -eq 120 ]; then echo "cc_recursion: ok (exit=120)";    else echo "cc_recursion: FAIL (exit=$$rc, expected 120)"; exit 1; fi
	@$(OUT)/cc_phi_swap_exe;  rc=$$?; if [ $$rc -eq 21 ];  then echo "cc_phi_swap:  ok (exit=21)";     else echo "cc_phi_swap:  FAIL (exit=$$rc, expected 21)";  exit 1; fi
	@$(OUT)/cc_fwd_struct_exe; rc=$$?; if [ $$rc -eq 42 ]; then echo "cc_fwd_struct: ok (exit=42)";    else echo "cc_fwd_struct: FAIL (exit=$$rc, expected 42)"; exit 1; fi
	@$(OUT)/cc_typedef_enum_exe; rc=$$?; if [ $$rc -eq 2 ]; then echo "cc_typedef_enum: ok (exit=2)";  else echo "cc_typedef_enum: FAIL (exit=$$rc, expected 2)"; exit 1; fi
	@$(OUT)/cc_proto_unnamed_exe; rc=$$?; if [ $$rc -eq 2 ]; then echo "cc_proto_unnamed: ok (exit=2)"; else echo "cc_proto_unnamed: FAIL (exit=$$rc, expected 2)"; exit 1; fi
	@$(OUT)/cc_bool_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_bool:      ok (exit=1)";            else echo "cc_bool:      FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_ifdef_aarch64_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_ifdef_aarch64: ok (exit=1)"; else echo "cc_ifdef_aarch64: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_size_t_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_size_t:   ok (exit=1)";          else echo "cc_size_t:   FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_adj_string_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_adj_string: ok (exit=1)";      else echo "cc_adj_string: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_struct_multi_decl_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_struct_multi_decl: ok (exit=1)"; else echo "cc_struct_multi_decl: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_global_struct_table_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_global_struct_table: ok (exit=1)"; else echo "cc_global_struct_table: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_local_static_assert_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_local_static_assert: ok (exit=1)"; else echo "cc_local_static_assert: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_fwd_struct_offsetof_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_fwd_struct_offsetof: ok (exit=1)"; else echo "cc_fwd_struct_offsetof: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_macro_paren_obj_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_macro_paren_obj: ok (exit=1)"; else echo "cc_macro_paren_obj: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_anon_nested_struct_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_anon_nested_struct: ok (exit=1)"; else echo "cc_anon_nested_struct: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_multi_include_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_multi_include: ok (exit=1)"; else echo "cc_multi_include: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@$(OUT)/cc_inline_asm_a64_exe; rc=$$?; if [ $$rc -eq 1 ]; then echo "cc_inline_asm_a64: ok (exit=1)"; else echo "cc_inline_asm_a64: FAIL (exit=$$rc, expected 1)"; exit 1; fi
	@out=$$($(OUT)/cc_string_exe 2>&1); rc=$$?; \
		if [ "$$out" = "hi a64" ] && [ $$rc -eq 0 ]; then \
			echo "cc_string:    ok (stdout=\"hi a64\", exit=0)"; \
		else \
			echo "cc_string:    FAIL (stdout=\"$$out\", exit=$$rc)"; exit 1; fi

# crt0 emitted by cc-a64 itself.
$(OUT)/crt0.o: $(OUT)/cc-a64
	@$(OUT)/cc-a64 --crt0 -o $@ >/dev/null

# --- Self-hosted toolchain end-to-end test ---
# Build a 2-object archive with ar-a64, link it with the main object via
# ld-a64, run the result. quadruple(10) = double_it(double_it(10)) = 40.

$(OUT)/cc_lib_a.o: tests/cc_lib_a.c $(OUT)/cc-a64
	@$(OUT)/cc-a64 -c $< -o $@ >/dev/null

$(OUT)/cc_lib_b.o: tests/cc_lib_b.c $(OUT)/cc-a64
	@$(OUT)/cc-a64 -c $< -o $@ >/dev/null

$(OUT)/cc_uses_lib.o: tests/cc_uses_lib.c $(OUT)/cc-a64
	@$(OUT)/cc-a64 -c $< -o $@ >/dev/null

$(OUT)/lib_helper.a: $(OUT)/cc_lib_a.o $(OUT)/cc_lib_b.o $(OUT)/ar-a64
	@$(OUT)/ar-a64 rcs $@ $(OUT)/cc_lib_a.o $(OUT)/cc_lib_b.o >/dev/null 2>&1

$(OUT)/cc_uses_lib_exe: $(OUT)/cc_uses_lib.o $(OUT)/lib_helper.a \
                       $(OUT)/crt0.o $(OUT)/crt0_stubs.o $(OUT)/ld-a64
	@$(OUT)/ld-a64 -o $@ $(OUT)/crt0.o $(OUT)/cc_uses_lib.o \
		$(OUT)/lib_helper.a $(OUT)/crt0_stubs.o >/dev/null

test-toolchain: $(OUT)/cc_uses_lib_exe
	@$(OUT)/cc_uses_lib_exe; rc=$$?; \
		if [ $$rc -eq 40 ]; then echo "test-toolchain: ok (exit=40)"; \
		else echo "test-toolchain: FAIL (exit=$$rc, expected 40)"; exit 1; fi

$(OUT)/cc_dup_foo_a.o: tests/cc_dup_foo_a.c $(OUT)/cc-a64
	@$(OUT)/cc-a64 -c $< -o $@ >/dev/null

$(OUT)/cc_dup_foo_b.o: tests/cc_dup_foo_b.c $(OUT)/cc-a64
	@$(OUT)/cc-a64 -c $< -o $@ >/dev/null

$(OUT)/cc_dup_foo_main.o: tests/cc_dup_foo_main.c $(OUT)/cc-a64
	@$(OUT)/cc-a64 -c $< -o $@ >/dev/null

$(OUT)/libdup.a: $(OUT)/cc_dup_foo_a.o $(OUT)/cc_dup_foo_b.o $(OUT)/ar-a64
	@$(OUT)/ar-a64 rcs $@ $(OUT)/cc_dup_foo_a.o $(OUT)/cc_dup_foo_b.o >/dev/null 2>&1

$(OUT)/cc_dup_foo_exe: $(OUT)/cc_dup_foo_main.o $(OUT)/libdup.a \
                      $(OUT)/crt0.o $(OUT)/crt0_stubs.o $(OUT)/ld-a64
	@$(OUT)/ld-a64 -o $@ $(OUT)/crt0.o $(OUT)/cc_dup_foo_main.o \
		$(OUT)/libdup.a $(OUT)/crt0_stubs.o >/dev/null

test-archive: $(OUT)/cc_dup_foo_exe
	@$(OUT)/cc_dup_foo_exe; rc=$$?; \
		if [ $$rc -eq 1 ]; then echo "test-archive: ok (exit=1)"; \
		else echo "test-archive: FAIL (exit=$$rc, expected 1)"; exit 1; fi

test: test-encode test-elf test-obj test-crt0 test-cc test-toolchain test-archive

# --- libc_a64.a — runtime library for cc-a64 outputs ---
# Most files are symlinks into ../stage07-cross/libc_x64/ since they're
# host-independent C.  syscalls.c is a64-specific.  fpu_ops.c uses
# `double`/`float` which our HIR codegen doesn't yet support, so we
# build it with host gcc (which targets AArch64 directly on this host).

LIBC_A64_HIR_SRCS = libc_a64/syscalls.c libc_a64/string.c libc_a64/malloc.c \
                   libc_a64/misc.c libc_a64/stdio.c
LIBC_A64_HIR_OBJS = $(patsubst libc_a64/%.c,$(OUT)/libc_a64_%.o,$(LIBC_A64_HIR_SRCS))

$(OUT)/libc_a64_%.o: libc_a64/%.c $(OUT)/cc-a64
	@$(OUT)/cc-a64 --hir -c $< -o $@ >/dev/null

$(OUT)/libc_a64_fpu_ops.o: ../stage07-cross/libc_x64/fpu_ops.c
	gcc -O2 -fno-builtin -fno-stack-protector -ffreestanding -c $< -o $@

# math_stubs.o — sqrt/sqrtf wrappers for fpu_ops.c.  Compiled with host gcc
# so __builtin_sqrt[f] lowers to hardware FSQRT.
$(OUT)/libc_a64_math_stubs.o: libc_a64/math_stubs.c
	gcc -O2 -fno-builtin -fno-stack-protector -ffreestanding -c $< -o $@

$(OUT)/libc_a64.a: $(LIBC_A64_HIR_OBJS) $(OUT)/libc_a64_fpu_ops.o $(OUT)/libc_a64_math_stubs.o $(OUT)/ar-a64
	@$(OUT)/ar-a64 rcs $@ $(LIBC_A64_HIR_OBJS) $(OUT)/libc_a64_fpu_ops.o $(OUT)/libc_a64_math_stubs.o >/dev/null 2>&1

libc: $(OUT)/libc_a64.a

# --- s32fast-hir — the SLOW-32 emulator, compiled by cc-a64 --hir ---
# Source is shared with stage07-cross (s32-fast-x64.c — the name is
# historical; the C is portable).

$(OUT)/s32fast.o: ../stage07-cross/s32-fast-x64.c $(OUT)/cc-a64
	@$(OUT)/cc-a64 --hir -c $< -o $@ >/dev/null

$(OUT)/s32fast-hir: $(OUT)/s32fast.o $(OUT)/crt0.o $(OUT)/libc_a64.a $(OUT)/ld-a64
	@$(OUT)/ld-a64 -o $@ $(OUT)/crt0.o $(OUT)/s32fast.o $(OUT)/libc_a64.a >/dev/null

s32fast: $(OUT)/s32fast-hir

BENCH_S32X ?= $(HOME)/slow-32/benchmark_core.s32x

bench: $(OUT)/s32fast-hir
	@if [ ! -f "$(BENCH_S32X)" ]; then \
		echo "Benchmark not found: $(BENCH_S32X)"; \
		echo "Set BENCH_S32X=path/to/benchmark_core.s32x"; \
		exit 1; \
	fi
	time $(OUT)/s32fast-hir $(BENCH_S32X)

clean:
	rm -f $(OUT)/test_encode $(OUT)/test_elf $(OUT)/test_obj $(OUT)/test_obj_global \
	      $(OUT)/test_crt0 $(OUT)/test_exit42 \
	      $(OUT)/test_obj.o $(OUT)/test_obj_exe \
	      $(OUT)/test_obj_global.o $(OUT)/test_obj_global_exe \
	      $(OUT)/crt0.o $(OUT)/crt0_main.o $(OUT)/crt0_stubs.o $(OUT)/test_crt0_prog \
	      $(OUT)/cc-a64 \
	      $(OUT)/cc_hello.o $(OUT)/cc_hello_exe \
	      $(OUT)/cc_locals.o $(OUT)/cc_locals_exe \
	      $(OUT)/cc_loop.o $(OUT)/cc_loop_exe \
	      $(OUT)/cc_recursion.o $(OUT)/cc_recursion_exe \
	      $(OUT)/cc_phi_swap.o $(OUT)/cc_phi_swap_exe \
	      $(OUT)/cc_fwd_struct.o $(OUT)/cc_fwd_struct_exe \
	      $(OUT)/cc_typedef_enum.o $(OUT)/cc_typedef_enum_exe \
	      $(OUT)/cc_proto_unnamed.o $(OUT)/cc_proto_unnamed_exe \
	      $(OUT)/cc_bool.o $(OUT)/cc_bool_exe \
	      $(OUT)/cc_ifdef_aarch64.o $(OUT)/cc_ifdef_aarch64_exe \
	      $(OUT)/cc_size_t.o $(OUT)/cc_size_t_exe \
	      $(OUT)/cc_adj_string.o $(OUT)/cc_adj_string_exe \
	      $(OUT)/cc_struct_multi_decl.o $(OUT)/cc_struct_multi_decl_exe \
	      $(OUT)/cc_global_struct_table.o $(OUT)/cc_global_struct_table_exe \
	      $(OUT)/cc_local_static_assert.o $(OUT)/cc_local_static_assert_exe \
	      $(OUT)/cc_fwd_struct_offsetof.o $(OUT)/cc_fwd_struct_offsetof_exe \
	      $(OUT)/cc_macro_paren_obj.o $(OUT)/cc_macro_paren_obj_exe \
	      $(OUT)/cc_anon_nested_struct.o $(OUT)/cc_anon_nested_struct_exe \
	      $(OUT)/cc_multi_include.o $(OUT)/cc_multi_include_exe \
	      $(OUT)/cc_string.o $(OUT)/cc_string_exe \
	      $(OUT)/ld-a64 $(OUT)/ar-a64 \
	      $(OUT)/cc_lib_a.o $(OUT)/cc_lib_b.o $(OUT)/cc_uses_lib.o \
	      $(OUT)/lib_helper.a $(OUT)/cc_uses_lib_exe \
	      $(OUT)/cc_dup_foo_a.o $(OUT)/cc_dup_foo_b.o \
	      $(OUT)/cc_dup_foo_main.o $(OUT)/libdup.a $(OUT)/cc_dup_foo_exe \
	      tests/ref.o tests/ref.bin
