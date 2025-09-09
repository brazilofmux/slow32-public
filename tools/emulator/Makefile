CC = gcc
CFLAGS = -Wall -Wextra -O2 -fomit-frame-pointer -g
TARGETS = slow32 slow32-fast

OBJS = slow32.o mmio_ring.o

all: $(TARGETS)

slow32: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

slow32-fast: slow32-fast.c slow32.h mmio_ring.o
	$(CC) -O3 -o $@ slow32-fast.c mmio_ring.o -I.

%.o: %.c slow32.h slow32_format.h
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGETS)

.PHONY: all clean benchmark
