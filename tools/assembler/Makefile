CC = gcc
CFLAGS = -Wall -Wextra -O2 -g
TARGETS = slow32asm

all: $(TARGETS)

slow32asm: slow32asm.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(TARGETS)

.PHONY: all clean