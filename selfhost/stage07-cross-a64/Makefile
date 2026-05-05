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

.PHONY: all test test-encode test-elf test-obj clean

all: test-encode test-elf test-obj

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

test: test-encode test-elf test-obj

clean:
	rm -f $(OUT)/test_encode $(OUT)/test_elf $(OUT)/test_obj $(OUT)/test_obj_global \
	      $(OUT)/test_exit42 $(OUT)/test_obj.o $(OUT)/test_obj_exe \
	      $(OUT)/test_obj_global.o $(OUT)/test_obj_global_exe \
	      tests/ref.o tests/ref.bin
