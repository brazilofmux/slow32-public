# Stage 07: Subset-C Linker (s32-ld.c)
# Compiled by Stage 04 cc.fth, assembled by Stage 05 s32-as.s32x,
# linked by Stage 03 link.fth (one last time — final Forth dependency).
# After this stage, Forth tools (stage01-03) are no longer needed.

.PHONY: all verify clean

all: s32-ld.s32x

# Build the linker executable
s32-ld.s32x: s32-ld.c ../stage05/s32-as.s32x ../stage05/libc/start.c ../stage05/libc/stdio.c ../stage05/libc/string_extra.c ../stage05/libc/convert.c ../stage05/crt0.s ../stage05/mmio_no_start.s
	@echo "Building s32-ld.s32x..."
	@bash build-s32-ld.sh

verify: s32-ld.s32x
	@echo "--- Stage 07: linker smoke test ---"
	@bash run-spike.sh

clean:
	rm -f s32-ld.s32x
