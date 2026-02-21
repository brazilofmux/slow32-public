# Stage 04: s12cc AST-based C Compiler + Tools (as, ar, ld)
# s12cc compiled by Stage 03 s32cc.s32x, tools compiled by s12cc.
# Assembled by Stage 03 s32-as.s32x, linked by Stage 03 s32-ld.s32x.
# Includes own libc, crt0, mmio_no_start sources for self-sufficiency.

.PHONY: all verify clean

BOOTSTRAP_CC = $(or $(wildcard ../stage03/cc.s32x),../stage03/s32cc.s32x)

all: cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x

# Build the compiler executable
s12cc.s32x: s12cc.c c_lexer_gen.c ast.h parser.h sema.h optimize.h codegen.h pp.h \
            $(BOOTSTRAP_CC) ../stage03/s32-as.s32x ../stage03/s32-ld.s32x \
            libc/start.c libc/stdio.c libc/string_extra.c libc/string_more.c \
            libc/ctype.c libc/convert.c libc/malloc.c \
            crt0.s mmio_no_start.s
	@echo "Building s12cc.s32x..."
	@bash build-s12cc.sh

cc.s32x: s12cc.s32x
	@cp -f s12cc.s32x cc.s32x

# Build tools (as, ar, ld) using s12cc
s32-as.s32x s32-ar.s32x s32-ld.s32x: cc.s32x \
            s32-as.c s32_formats_min.h s32-ar.c s32ar_min.h s32-ld.c \
            ../stage03/s32-as.s32x ../stage03/s32-ld.s32x \
            libc/start.c libc/stdio.c libc/string_extra.c libc/convert.c \
            crt0.s mmio_no_start.s
	@echo "Building tools (as, ar, ld)..."
	@bash build-tools.sh

verify: cc.s32x s12cc.s32x
	@echo "--- Stage 04: s12cc compiler tests ---"
	@bash run-tests.sh

clean:
	rm -f cc.s32x s12cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x
