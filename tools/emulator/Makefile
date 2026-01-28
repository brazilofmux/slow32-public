CC = gcc
CFLAGS = -Wall -Wextra -O3 -fomit-frame-pointer -g
TARGETS = slow32 slow32-fast

HEADERS = slow32.h memory_manager.h s32x_loader.h mmio_ring.h

OBJS = slow32.o mmio_ring.o

all: $(TARGETS)

slow32: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

slow32-fast: slow32-fast.c $(HEADERS) mmio_ring.o
	$(CC) $(CFLAGS) -o $@ slow32-fast.c mmio_ring.o -I.

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGETS)

.PHONY: all clean
