CC = gcc
CFLAGS = -Wall -Wextra -O2
TARGETS = slow32dump slow32dis s32-ar

all: $(TARGETS)

slow32dump: slow32dump.c
	$(CC) $(CFLAGS) $< -o $@

slow32dis: slow32dis.c
	$(CC) $(CFLAGS) $< -o $@

s32-ar: s32-ar.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f $(TARGETS)

.PHONY: all clean
