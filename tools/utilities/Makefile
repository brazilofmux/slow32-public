CC = gcc
CFLAGS = -Wall -Wextra -O2
TARGETS = s32-objdump s32-exedump slow32dis s32-ar

all: $(TARGETS)

s32-objdump: s32-objdump.c
	$(CC) $(CFLAGS) $< -o $@

s32-exedump: s32-exedump.c
	$(CC) $(CFLAGS) $< -o $@

slow32dis: slow32dis.c
	$(CC) $(CFLAGS) $< -o $@

s32-ar: s32-ar.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f $(TARGETS)

.PHONY: all clean