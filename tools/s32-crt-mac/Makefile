CC = clang
CFLAGS = -fobjc-arc -O2 -Wall -Wextra
LDFLAGS = -framework Cocoa

s32-crt-mac: s32-crt-mac.m
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

clean:
	rm -f s32-crt-mac

.PHONY: clean
