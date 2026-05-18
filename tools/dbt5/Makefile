# SLOW-32 DBT5 — Stage 5 only (clean-room experimental compiler)
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
CFLAGS += -I.                    # local headers (slow32_inst.h, stage5_*.h, etc.)
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
    # No A64 codegen backend yet — this is the whole point of the fork.
    # We will add stage5_codegen_a64.c (or equivalent) when we write the real one.
    STAGE5_ARCH_SRCS =
else
    EMIT_SRCS = emit_x64.c
    # Legacy coupled codegen (being migrated).  We also compile the new
    # clean placeholder so an x86-64 agent has a visible target.
    STAGE5_ARCH_SRCS = stage5_codegen.c stage5_codegen_x64.c
endif

# Stage 5 compiler pipeline (the actual experiment)
STAGE5_SRCS = stage5_lift.c stage5_burg.c stage5_ssa.c stage5_ra.c \
              stage5_mir.c stage5_lir_opt.c $(STAGE5_ARCH_SRCS)

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
