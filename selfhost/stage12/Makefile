# Stage 12: s12cc AST-based C Compiler (Ragel lexer + recursive-descent parser)
# Compiled by Stage 11 s32cc.s32x, assembled by Stage 05 s32-as.s32x,
# linked by Stage 07 s32-ld.s32x.

.PHONY: all verify clean

all: s12cc.s32x

# Build the compiler executable
s12cc.s32x: s12cc.c c_lexer_gen.c ast.h parser.h codegen.h pp.h ../stage11/s32cc.s32x ../stage05/s32-as.s32x ../stage07/s32-ld.s32x ../stage05/libc/start.c ../stage05/libc/stdio.c ../stage05/libc/string_extra.c ../stage05/libc/string_more.c ../stage05/libc/ctype.c ../stage05/libc/convert.c ../stage05/libc/malloc.c ../stage05/crt0.s ../stage05/mmio_no_start.s
	@echo "Building s12cc.s32x..."
	@bash build-s12cc.sh

verify: s12cc.s32x
	@echo "--- Stage 12: s12cc compiler tests ---"
	@bash run-tests.sh

clean:
	rm -f s12cc.s32x
