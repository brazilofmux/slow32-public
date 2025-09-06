CC = gcc
CFLAGS = -Wall -Wextra -O2 -fomit-frame-pointer -g
TARGETS = slow32 slow32-fast

OBJS = slow32.o

all: $(TARGETS)

slow32: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

slow32-fast: slow32-fast.c slow32.h
	$(CC) -O3 -o $@ $< -I.

%.o: %.c slow32.h slow32_format.h
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGETS)

.PHONY: all clean benchmark
