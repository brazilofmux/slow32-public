# Stage 12: s12cc AST-based C Compiler (Ragel lexer + recursive-descent parser)
# Compiled by Stage 11 s32cc.s32x, assembled by Stage 02 s32-as.s32x,
# linked by Stage 02 s32-ld.s32x.

.PHONY: all verify clean

all: s12cc.s32x

# Build the compiler executable
s12cc.s32x: s12cc.c c_lexer_gen.c ast.h parser.h sema.h optimize.h codegen.h pp.h ../stage11/s32cc.s32x ../stage02/s32-as.s32x ../stage02/s32-ld.s32x ../stage02/libc/start.c ../stage02/libc/stdio.c ../stage02/libc/string_extra.c ../stage02/libc/string_more.c ../stage02/libc/ctype.c ../stage02/libc/convert.c ../stage02/libc/malloc.c ../stage02/crt0.s ../stage02/mmio_no_start.s
	@echo "Building s12cc.s32x..."
	@bash build-s12cc.sh

verify: s12cc.s32x
	@echo "--- Stage 12: s12cc compiler tests ---"
	@bash run-tests.sh

clean:
	rm -f s12cc.s32x
