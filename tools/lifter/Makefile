CC ?= gcc
CFLAGS = -O2 -Wall -Wextra -Wno-unused-parameter

# LLVM tools - system llc/opt with x86-64 backend
# The custom ~/llvm-project build only has the SLOW-32 backend
OPT ?= opt-19
LLC ?= llc-19

.PHONY: all clean

all: s32-lift

s32-lift: s32_lift.c ../../common/s32_formats.h
	$(CC) $(CFLAGS) -o $@ $< -I../../common

runtime.o: runtime.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f s32-lift runtime.o *.ll *.bc *.o lifted_*

# ---- Convenience targets for lifting and running ----
# Usage: make lift PROG=../../regression/results/feature-loops/test.s32x
# Usage: make run  PROG=../../regression/results/feature-loops/test.s32x

lift: s32-lift
	./s32-lift $(PROG) -o lifted.ll

compile: lift runtime.o
	$(LLC) -O2 lifted.ll -filetype=obj -o lifted.o
	$(CC) lifted.o runtime.o -o lifted_native -lm

run: compile
	./lifted_native
