# Stage 07: Subset-C Linker (s32-ld.c)
# Compiled by Stage 04 cc.fth, assembled by Stage 02 s32-as.s32x,
# linked by Stage 03 link.fth (one last time — final Forth dependency).
# After this stage, Forth tools (stage01-03) are no longer needed.

.PHONY: all verify clean

all: s32-ld.s32x

# Build the linker executable
s32-ld.s32x: s32-ld.c ../stage02/s32-as.s32x ../stage02/libc/start.c ../stage02/libc/stdio.c ../stage02/libc/string_extra.c ../stage02/libc/convert.c ../stage02/crt0.s ../stage02/mmio_no_start.s
	@echo "Building s32-ld.s32x..."
	@bash build-s32-ld.sh

verify: s32-ld.s32x
	@echo "--- Stage 07: linker smoke test ---"
	@bash run-spike.sh

clean:
	rm -f s32-ld.s32x
