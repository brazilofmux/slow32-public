CC = gcc
CFLAGS = -O2 -Wall -I../common
TARGETS = s32-objdump s32-exedump slow32dis

all: $(TARGETS)

s32-objdump: s32-objdump.c ../common/s32_formats.h
	$(CC) $(CFLAGS) s32-objdump.c -o s32-objdump

s32-exedump: s32-exedump.c ../common/s32_formats.h
	$(CC) $(CFLAGS) s32-exedump.c -o s32-exedump

slow32dis: slow32dis.c
	$(CC) -O2 -Wall slow32dis.c -o slow32dis

clean:
	rm -f $(TARGETS)

.PHONY: all clean