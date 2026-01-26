# Benchmark suite Makefile
SLOW32_ROOT := ..
CLANG := ~/llvm-project/build/bin/clang
LLC := ~/llvm-project/build/bin/llc
ASM := $(SLOW32_ROOT)/tools/assembler/slow32asm
LD := $(SLOW32_ROOT)/tools/linker/s32-ld

RUNTIME := $(SLOW32_ROOT)/runtime
CRT0 := $(RUNTIME)/crt0.s32o
LIBS32 := $(RUNTIME)/libs32.s32a
LIBC := $(RUNTIME)/libc_debug.s32a

CFLAGS := -target slow32-unknown-none -S -emit-llvm -O2 -I$(RUNTIME)/include
LDFLAGS :=

BENCHMARKS := bench-sum bench-matrix bench-sort bench-fib bench-prime

all: $(BENCHMARKS:=.s32x)

# Explicit rules to avoid pattern rule issues
bench-sum.s32x: bench-sum.s32o
	$(LD) -o $@ $(CRT0) $< $(LIBC) $(LIBS32)

bench-matrix.s32x: bench-matrix.s32o
	$(LD) -o $@ $(CRT0) $< $(LIBC) $(LIBS32)

bench-sort.s32x: bench-sort.s32o
	$(LD) -o $@ $(CRT0) $< $(LIBC) $(LIBS32)

bench-fib.s32x: bench-fib.s32o
	$(LD) -o $@ $(CRT0) $< $(LIBC) $(LIBS32)

bench-prime.s32x: bench-prime.s32o
	$(LD) -o $@ $(CRT0) $< $(LIBC) $(LIBS32)

%.ll: %.c
	$(CLANG) $(CFLAGS) $< -o $@

%.s: %.ll
	$(LLC) -mtriple=slow32-unknown-none $< -o $@

%.s32o: %.s
	$(ASM) $< $@

clean:
	rm -f *.ll *.s *.s32o *.s32x

.PHONY: all clean
.PRECIOUS: %.ll %.s %.s32o
