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

.PHONY: all test test-encode clean

all: test-encode

# --- Encoder verification ---
# Diff our a64_encode output against a gas-assembled reference.

$(OUT)/test_encode: tests/test_encode.c a64_encode.h tests/ref.bin
	gcc -O2 -Wall -Wno-unused-function -o $@ tests/test_encode.c

tests/ref.bin: tests/ref.s
	tests/gen_ref.sh

test-encode: $(OUT)/test_encode
	@$(OUT)/test_encode

test: test-encode

clean:
	rm -f $(OUT)/test_encode tests/ref.o tests/ref.bin
