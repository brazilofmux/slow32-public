# SLOW-32 DBT Makefile
# Multi-architecture: detects x86-64 or AArch64 at build time

CC = gcc
CFLAGS = -Wall -Wextra -O2 -g -std=c11
CFLAGS += -D_GNU_SOURCE -D_POSIX_C_SOURCE=200809L
CFLAGS += -I../emulator  # For s32x_loader.h

# Disable some warnings that are noisy during development
CFLAGS += -Wno-unused-parameter -Wno-unused-variable

LDFLAGS = -lm

# Architecture detection: select emitter and translator
UNAME_M := $(shell uname -m)
ifeq ($(UNAME_M),aarch64)
    ARCH_SRCS = emit_a64.c translate_a64.c
    ARCH_HDRS = emit_a64.h
else
    ARCH_SRCS = emit_x64.c translate.c
    ARCH_HDRS = emit_x64.h
endif

# Source files
SRCS = dbt.c $(ARCH_SRCS) block_cache.c stage5_lift.c stage5_burg.c
EMULATOR_SRCS = ../emulator/mmio_ring.c
OBJS = $(SRCS:.c=.o)
EMULATOR_OBJS = mmio_ring.o

# Headers
HDRS = cpu_state.h $(ARCH_HDRS) translate.h block_cache.h dbt_limits.h \
	stage5_lift.h stage5_burg.h

# Main target
TARGET = slow32-dbt

.PHONY: all clean test test-diff compare-safe-unsafe compare-safe-unsafe-strict stress-safe-unsafe sweep-peephole sweep-peephole-strict stage5-validate-preflight stage5-gate stage5-gate-fast debug disasm

all: $(TARGET)
	@echo "Built for $(UNAME_M)"

$(TARGET): $(OBJS) $(EMULATOR_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Build mmio_ring.o from emulator directory
mmio_ring.o: ../emulator/mmio_ring.c ../emulator/mmio_ring.h
	$(CC) $(CFLAGS) -c -o $@ $<

# Object files depend on headers
%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c -o $@ $<

# Test targets
test: $(TARGET)
	@echo "Running basic smoke test..."
	@if [ -f ../../regression/feature-arithmetic.s32x ]; then \
		./$(TARGET) -s ../../regression/feature-arithmetic.s32x && echo "PASS: arithmetic"; \
	else \
		echo "SKIP: regression tests not found"; \
	fi

# Run regression suite comparison
test-diff: $(TARGET)
	@echo "Running differential tests against interpreter..."
	@./scripts/diff-test.sh

# Repeated Stage5 safe-vs-unsafe stability stress.
# Usage examples:
#   make stress-safe-unsafe ITERS=200
#   make stress-safe-unsafe ITERS=50 TESTS="../../regression/results/feature-strtod/test.s32x"
ITERS ?= 100
TESTS ?=
SWEEP_ITERS ?= $(ITERS)
STRESS_ITERS ?= $(ITERS)

compare-safe-unsafe: $(TARGET)
	@echo "Comparing safe-vs-unsafe Stage5 output..."
	@./scripts/compare-stage5-safe-unsafe.sh $(TESTS)

compare-safe-unsafe-strict: $(TARGET)
	@echo "Comparing safe-vs-unsafe Stage5 output (strict)..."
	@DBT_REQUIRE_MATCH=1 ./scripts/compare-stage5-safe-unsafe.sh $(TESTS)

stage5-validate-preflight: $(TARGET)
	@echo "Running Stage5 lift validator preflight..."
	@./scripts/stage5-validate-preflight.sh $(TESTS)

stress-safe-unsafe: $(TARGET)
	@echo "Running safe-vs-unsafe stress (iters=$(ITERS))..."
	@./scripts/stress-stage5-safe-unsafe.sh "$(ITERS)" $(TESTS)

# Sweep peephole call-guard modes and report mismatches.
# Usage examples:
#   make sweep-peephole ITERS=120
#   make sweep-peephole ITERS=80 TESTS=".../feature-strtod/test.s32x"
sweep-peephole: $(TARGET)
	@echo "Running peephole guard sweep (iters=$(ITERS))..."
	@./scripts/sweep-peephole-call-guards.sh "$(ITERS)" $(TESTS)

# Strict sweep: non-zero if any mode is unstable.
sweep-peephole-strict: $(TARGET)
	@echo "Running strict peephole guard sweep (iters=$(ITERS))..."
	@SWEEP_REQUIRE_STABLE=1 ./scripts/sweep-peephole-call-guards.sh "$(ITERS)" $(TESTS)

# One-shot strict gate for Stage5 peephole/side-exit safety.
# Usage examples:
#   make stage5-gate
#   make stage5-gate TESTS="../../regression/results/feature-strtod/test.s32x" SWEEP_ITERS=80 STRESS_ITERS=200
stage5-gate: $(TARGET)
	@echo "Stage5 gate: lift validator preflight"
	@./scripts/stage5-validate-preflight.sh $(TESTS)
	@echo "Stage5 gate: strict compare"
	@SLOW32_DBT_STAGE5_VALIDATE_LIFT=1 SLOW32_DBT_STAGE5_VALIDATE_REQUIRE=1 \
	 DBT_REQUIRE_MATCH=1 ./scripts/compare-stage5-safe-unsafe.sh $(TESTS)
	@echo "Stage5 gate: strict peephole sweep (iters=$(SWEEP_ITERS))"
	@SLOW32_DBT_STAGE5_VALIDATE_LIFT=1 SLOW32_DBT_STAGE5_VALIDATE_REQUIRE=1 \
	 SWEEP_REQUIRE_STABLE=1 ./scripts/sweep-peephole-call-guards.sh "$(SWEEP_ITERS)" $(TESTS)
	@echo "Stage5 gate: stress safe-vs-unsafe (iters=$(STRESS_ITERS))"
	@SLOW32_DBT_STAGE5_VALIDATE_LIFT=1 SLOW32_DBT_STAGE5_VALIDATE_REQUIRE=1 \
	 ./scripts/stress-stage5-safe-unsafe.sh "$(STRESS_ITERS)" $(TESTS)

# Quick local preflight for rapid iteration.
stage5-gate-fast: SWEEP_ITERS=20
stage5-gate-fast: STRESS_ITERS=20
stage5-gate-fast: stage5-gate

clean:
	rm -f *.o $(TARGET)

# Debug build with more checks
debug: CFLAGS += -fsanitize=address,undefined -O0
debug: LDFLAGS += -fsanitize=address,undefined
debug: $(TARGET)

# Disassemble generated code for debugging
disasm: $(TARGET)
	objdump -d $(TARGET) | less
