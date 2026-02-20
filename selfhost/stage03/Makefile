# Stage 03: s32-cc Compiler + Tools (as, ar, ld)
# s32cc compiled by Stage 02 cc-min.s32x, tools compiled by s32cc.
# Assembled by Stage 02 s32-as.s32x, linked by Stage 02 s32-ld.s32x.
# Includes own libc, crt0, mmio_no_start sources for self-sufficiency.

.PHONY: all verify clean

all: s32cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x

# Build the compiler executable
s32cc.s32x: s32cc.c s32cc_lex.h s32cc_parse.h \
            ../stage02/cc-min.s32x ../stage02/s32-as.s32x ../stage02/s32-ld.s32x \
            libc/start.c libc/stdio.c libc/string_extra.c libc/convert.c \
            crt0.s mmio_no_start.s
	@echo "Building s32cc.s32x..."
	@bash build-s32cc.sh

# Build tools (as, ar, ld) using s32cc
s32-as.s32x s32-ar.s32x s32-ld.s32x: s32cc.s32x \
            s32-as.c s32_formats_min.h s32-ar.c s32ar_min.h s32-ld.c \
            ../stage02/s32-as.s32x ../stage02/s32-ld.s32x \
            libc/start.c libc/stdio.c libc/string_extra.c libc/convert.c \
            crt0.s mmio_no_start.s
	@echo "Building tools (as, ar, ld)..."
	@bash build-tools.sh

verify: s32cc.s32x
	@echo "--- Stage 03: s32-cc compiler tests ---"
	@bash run-spike.sh

clean:
	rm -f s32cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x
