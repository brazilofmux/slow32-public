# Stage 0 Bootstrap Emulator for SLOW-32
#
# Build:  make
# Clean:  make clean
# Test:   make test  (requires the Forth kernel to be built)

CC     ?= cc
CFLAGS ?= -O2 -Wall -Wextra -std=c11 -pedantic
TARGET  = s32-emu

all: $(TARGET)

$(TARGET): s32-emu.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(TARGET)

# Smoke test: run the Forth kernel and feed it a simple expression
test: $(TARGET)
	@echo "=== Stage 0 bootstrap emulator smoke test ==="
	@echo "1 2 + . CR BYE" | ./$(TARGET) ../../../forth/kernel.s32x 2>/dev/null | grep -q "3" && \
		echo "PASS: Forth kernel runs, 1+2=3" || \
		echo "FAIL: Forth kernel did not produce expected output"

.PHONY: all clean test
