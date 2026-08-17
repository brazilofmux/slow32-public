CC = gcc
CFLAGS = -Wall -Wextra -O2

all: s32-crt

s32-crt: s32-crt.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f s32-crt

.PHONY: all clean
