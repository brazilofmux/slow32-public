CC = gcc
CFLAGS = -O2 -Wall -I../../common
TARGET = s32-ld

all: $(TARGET)

s32-ld: s32-ld.c ../../common/s32_formats.h
	$(CC) $(CFLAGS) s32-ld.c -o s32-ld

clean:
	rm -f $(TARGET)

.PHONY: all clean