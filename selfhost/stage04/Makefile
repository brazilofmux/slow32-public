# Stage 04: Compiler + Tools (as, ar, ld)
# compiler compiled by Stage 03 cc.s32x, tools compiled by compiler.
# Assembled by Stage 03 s32-as.s32x, linked by Stage 03 s32-ld.s32x.
# Includes own libc, crt0, mmio_no_start sources for self-sufficiency.

.PHONY: all verify clean

BOOTSTRAP_CC = ../stage03/cc.s32x

all: cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x

# Build the compiler executable
cc.s32x: s12cc.c c_lexer_gen.c ast.h parser.h sema.h optimize.h codegen.h pp.h \
         $(BOOTSTRAP_CC) ../stage03/s32-as.s32x ../stage03/s32-ld.s32x \
         libc/start.c libc/stdio.c libc/string_extra.c libc/string_more.c \
         libc/ctype.c libc/convert.c libc/malloc.c \
         crt0.s mmio_no_start.s
	@echo "Building cc.s32x..."
	@bash build-s12cc.sh

# Build tools (as, ar, ld) using compiler
s32-as.s32x s32-ar.s32x s32-ld.s32x: cc.s32x \
            s32-as.c s32_formats_min.h s32-ar.c s32ar_min.h s32-ld.c \
            ../stage03/s32-as.s32x ../stage03/s32-ld.s32x \
            libc/start.c libc/stdio.c libc/string_extra.c libc/convert.c \
            crt0.s mmio_no_start.s
	@echo "Building tools (as, ar, ld)..."
	@bash build-tools.sh

verify: cc.s32x
	@echo "--- Stage 04: compiler tests ---"
	@bash run-tests.sh

clean:
	rm -f cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x
