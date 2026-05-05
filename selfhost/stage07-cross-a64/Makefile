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

.PHONY: all test test-encode test-elf test-obj test-crt0 test-cc test-toolchain clean

all: $(OUT)/cc-a64 $(OUT)/ld-a64 $(OUT)/ar-a64 \
     test-encode test-elf test-obj test-crt0 test-cc test-toolchain

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

$(OUT)/cc_%_exe: $(OUT)/cc_%.o $(OUT)/crt0.o $(OUT)/crt0_stubs.o
	@ld -o $@ $(OUT)/crt0.o $< $(OUT)/crt0_stubs.o

test-cc: $(OUT)/cc_hello_exe $(OUT)/cc_locals_exe \
         $(OUT)/cc_loop_exe  $(OUT)/cc_recursion_exe \
         $(OUT)/cc_string_exe
	@$(OUT)/cc_hello_exe;     rc=$$?; if [ $$rc -eq 42 ];  then echo "cc_hello:     ok (exit=42)";     else echo "cc_hello:     FAIL (exit=$$rc, expected 42)";     exit 1; fi
	@$(OUT)/cc_locals_exe;    rc=$$?; if [ $$rc -eq 42 ];  then echo "cc_locals:    ok (exit=42)";     else echo "cc_locals:    FAIL (exit=$$rc, expected 42)";     exit 1; fi
	@$(OUT)/cc_loop_exe;      rc=$$?; if [ $$rc -eq 47 ];  then echo "cc_loop:      ok (exit=47)";     else echo "cc_loop:      FAIL (exit=$$rc, expected 47)";     exit 1; fi
	@$(OUT)/cc_recursion_exe; rc=$$?; if [ $$rc -eq 120 ]; then echo "cc_recursion: ok (exit=120)";    else echo "cc_recursion: FAIL (exit=$$rc, expected 120)"; exit 1; fi
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

test: test-encode test-elf test-obj test-crt0 test-cc test-toolchain

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
	      $(OUT)/cc_string.o $(OUT)/cc_string_exe \
	      $(OUT)/ld-a64 $(OUT)/ar-a64 \
	      $(OUT)/cc_lib_a.o $(OUT)/cc_lib_b.o $(OUT)/cc_uses_lib.o \
	      $(OUT)/lib_helper.a $(OUT)/cc_uses_lib_exe \
	      tests/ref.o tests/ref.bin
