# SLOW-32 DBT5 — Stage 5 only (clean-room experimental compiler)
#
# Post-MIR split (D1): two parallel experimental pipelines under target/
#   - target/x64/  — legacy path (Stage 4 x86-shaped, to be replaced)
#   - target/a64/  — new AArch64 experimental path (three-operand preferred)
#
# Pre-MIR (lift → SSA → MIR) lives under pre/ and is the last relatively
# target-agnostic layer.
#
# This tree is deliberately separated from the production ./tools/dbt (Stages 1-4).
# Rule: NO FILE IN THIS DIRECTORY MAY INCLUDE OR CALL ANYTHING FROM
#       ../dbt/translate*.c or ../dbt/translate.h.
#
# We use only:
#   - raw emit_a64 / emit_x64 primitives
#   - block_cache (for the eventual native block repository)
#   - cpu_state, dbt_limits, shadow_interp (for fallback / validation)
#   - the full Stage 5 pipeline (lift → SSA → MIR → BURG → LIR → RA → codegen)
#
# On AArch64 (current host): native code emission is not yet implemented.
# dbt5 can still run the complete analysis pipeline and fall back to
# shadow interpretation for execution.  This is the correct place to grow
# a real A64 Stage 5 codegen that never saw the Stage 4 helpers.

CC = gcc
CFLAGS = -Wall -Wextra -O2 -g -std=c11
CFLAGS += -D_GNU_SOURCE -D_POSIX_C_SOURCE=200809L
CFLAGS += -I.                    # local headers
CFLAGS += -Ipre/lift -Ipre/ssa -Ipre/mir
CFLAGS += -Itarget/x64/burg -Itarget/x64/lir -Itarget/x64/regalloc -Itarget/x64/codegen
CFLAGS += -Itarget/a64/burg -Itarget/a64/lir -Itarget/a64/regalloc -Itarget/a64/codegen
CFLAGS += -I../../common         # s32_formats.h
CFLAGS += -I../../emulator       # s32x_loader.h

ifeq ($(shell uname -s),Darwin)
    CFLAGS += -D_XOPEN_SOURCE=700 -D_DARWIN_C_SOURCE
endif

CFLAGS += -Wno-unused-parameter -Wno-unused-variable

LDFLAGS = -lm

# Architecture detection (same convention as production dbt)
UNAME_M := $(shell uname -m)
ifeq ($(UNAME_M),arm64)
    UNAME_M := aarch64
endif

ifeq ($(UNAME_M),aarch64)
    EMIT_SRCS = emit_a64.c
    # New experimental A64 pipeline (post-MIR split)
    TARGET_BURG = target/a64/burg/stage5_burg_a64.c
    TARGET_LIR  = target/a64/lir/stage5_lir_a64.h
    TARGET_RA   = target/a64/regalloc/stage5_ra.c
    TARGET_CODEGEN = target/a64/codegen/stage5_codegen_a64.c
else
    EMIT_SRCS = emit_x64.c
    # Legacy x86-64 pipeline (will be replaced later)
    TARGET_BURG = target/x64/burg/stage5_burg_x64.c
    TARGET_LIR  = target/x64/lir/stage5_lir_x64.h
    TARGET_RA   = target/x64/regalloc/stage5_ra.c
    TARGET_CODEGEN = target/x64/codegen/stage5_codegen_x64.c dbt5_fp_helper.c
endif

# Stage 5 compiler pipeline (the actual experiment)
# Split after MIR: pre/ is shared, target/ is per-pipeline
STAGE5_SRCS = pre/lift/stage5_lift.c \
              $(TARGET_BURG) \
              pre/ssa/stage5_ssa.c \
              $(TARGET_RA) \
              pre/mir/stage5_mir.c \
              pre/util/stage5_lir_opt.c \
              $(TARGET_CODEGEN)

# Core runtime pieces we still need (block cache, shadow interp fallback, emitters)
CORE_SRCS = dbt5.c block_cache.c shadow_interp.c slow32_inst.c $(EMIT_SRCS)

SRCS = $(CORE_SRCS) $(STAGE5_SRCS)
OBJS = $(SRCS:.c=.o)

# mmio_ring is only needed if we grow full MMIO support later; not for the compiler itself
# EMULATOR_SRCS = ../../emulator/mmio_ring.c

TARGET = slow32-dbt5

.PHONY: all clean

all: $(TARGET)
	@echo "Built SLOW-32 Stage 5 DBT (clean room) for $(UNAME_M)"
	@echo "  This binary contains ZERO Stage 1-4 translator code."

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f *.o $(TARGET)

# Quick smoke: try to lift + analyze a program (no native emission on arm64 yet)
smoke: $(TARGET)
	@echo "Stage 5 smoke (analysis only on this host)..."
	@if [ -f ../../regression/feature-arithmetic.s32x ]; then \
	  ./$(TARGET) ../../regression/feature-arithmetic.s32x --lift-only; \
	else \
	  echo "SKIP: no regression binary found"; \
	fi
