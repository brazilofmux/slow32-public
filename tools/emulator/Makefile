CC = gcc
CFLAGS = -Wall -Wextra -O3 -fomit-frame-pointer -g

include ../../common/s32_crypto.mk
CFLAGS += $(S32_CRYPTO_CFLAGS)
TARGETS = slow32 slow32-fast

HEADERS = slow32.h memory_manager.h s32x_loader.h mmio_ring.h

OBJS = slow32.o mmio_ring.o

all: $(TARGETS)

slow32: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ -lm $(S32_CRYPTO_LIBS)

slow32-fast: slow32-fast.c $(HEADERS) mmio_ring.o
	$(CC) $(CFLAGS) -o $@ slow32-fast.c mmio_ring.o -I. -lm $(S32_CRYPTO_LIBS)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGETS)

.PHONY: all clean
