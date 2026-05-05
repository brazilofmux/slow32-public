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

.PHONY: all test test-encode test-elf test-obj test-crt0 clean

all: test-encode test-elf test-obj test-crt0

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

test: test-encode test-elf test-obj test-crt0

clean:
	rm -f $(OUT)/test_encode $(OUT)/test_elf $(OUT)/test_obj $(OUT)/test_obj_global \
	      $(OUT)/test_crt0 $(OUT)/test_exit42 \
	      $(OUT)/test_obj.o $(OUT)/test_obj_exe \
	      $(OUT)/test_obj_global.o $(OUT)/test_obj_global_exe \
	      $(OUT)/crt0.o $(OUT)/crt0_main.o $(OUT)/crt0_stubs.o $(OUT)/test_crt0_prog \
	      tests/ref.o tests/ref.bin
