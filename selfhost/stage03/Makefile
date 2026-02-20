# Stage 03: s32-cc Compiler (lex/parse split, evolved from cc-min)
# Compiled by Stage 02 cc-min.s32x, assembled by Stage 02 s32-as.s32x,
# linked by Stage 02 s32-ld.s32x.

.PHONY: all verify clean

all: s32cc.s32x

# Build the compiler executable
s32cc.s32x: s32cc.c s32cc_lex.h s32cc_parse.h ../stage02/cc-min.s32x ../stage02/s32-as.s32x ../stage02/s32-ld.s32x ../stage02/libc/start.c ../stage02/libc/stdio.c ../stage02/libc/string_extra.c ../stage02/libc/convert.c ../stage02/crt0.s ../stage02/mmio_no_start.s
	@echo "Building s32cc.s32x..."
	@bash build-s32cc.sh

verify: s32cc.s32x
	@echo "--- Stage 03: s32-cc compiler tests ---"
	@bash run-spike.sh

clean:
	rm -f s32cc.s32x
