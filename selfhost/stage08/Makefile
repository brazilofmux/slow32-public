# Stage 08: Subset-C Compiler (cc-min)
# Compiled by Stage 04 cc.fth (one last time), assembled by Stage 05 s32-as.s32x,
# linked by Stage 07 s32-ld.s32x. After this stage, ALL Forth tools are retired.

.PHONY: all verify clean

all: cc-min.s32x

# Build the compiler executable
cc-min.s32x: cc-min.c cc-min-pass1.c cc-min-pass2.c cc-min-pass3.c ../stage05/s32-as.s32x ../stage07/s32-ld.s32x ../stage05/libc/start.c ../stage05/libc/stdio.c ../stage05/libc/string_extra.c ../stage05/libc/convert.c ../stage05/crt0.s ../stage05/mmio_no_start.s
	@echo "Building cc-min.s32x..."
	@bash build-cc-min.sh

verify: cc-min.s32x
	@echo "--- Stage 08: compiler smoke test ---"
	@bash run-spike.sh

clean:
	rm -f cc-min.s32x
