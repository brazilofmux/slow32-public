CC = gcc
CFLAGS = -O2 -Wall -I../common
TARGETS = slow32dump

all: $(TARGETS)

slow32dump: utilities/slow32dump.c ../common/s32_formats.h
	$(CC) $(CFLAGS) utilities/slow32dump.c -o utilities/slow32dump

clean:
	rm -f $(TARGETS)

.PHONY: all clean
