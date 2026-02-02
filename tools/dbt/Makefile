# SLOW-32 DBT Makefile
# Stage 1 - Correctness-first dynamic binary translator

CC = gcc
CFLAGS = -Wall -Wextra -O2 -g -std=c11
CFLAGS += -D_GNU_SOURCE -D_POSIX_C_SOURCE=200809L
CFLAGS += -I../emulator  # For s32x_loader.h

# Disable some warnings that are noisy during development
CFLAGS += -Wno-unused-parameter -Wno-unused-variable

LDFLAGS = -lm

# Source files
SRCS = dbt.c emit_x64.c translate.c block_cache.c
EMULATOR_SRCS = ../emulator/mmio_ring.c
OBJS = $(SRCS:.c=.o)
EMULATOR_OBJS = mmio_ring.o

# Headers
HDRS = cpu_state.h emit_x64.h translate.h block_cache.h

# Main target
TARGET = slow32-dbt

.PHONY: all clean test

all: $(TARGET)

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

clean:
	rm -f $(OBJS) $(EMULATOR_OBJS) $(TARGET)

# Debug build with more checks
debug: CFLAGS += -fsanitize=address,undefined -O0
debug: LDFLAGS += -fsanitize=address,undefined
debug: $(TARGET)

# Disassemble generated code for debugging
disasm: $(TARGET)
	objdump -d $(TARGET) | less

.PHONY: debug disasm
