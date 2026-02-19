# Stage 14: Compiler playground (copy of stage13, bootstrapped by stage13)
# All C sources compiled by Stage 13 s12cc.s32x, assembled by Stage 02 s32-as.s32x,
# linked by Stage 07 s32-ld.s32x.

COMPILER_SRCS = s12cc.c c_lexer_gen.c ast.h parser.h sema.h optimize.h codegen.h pp.h
LIBC_SRCS = libc/start.c libc/stdio.c libc/string_extra.c libc/string_more.c libc/ctype.c libc/convert.c libc/malloc.c
RUNTIME_SRCS = crt0.s mmio_no_start.s
TOOL_SRCS = tools/s32-as.c tools/s32_formats_min.h tools/s32-ar.c tools/s32ar_min.h tools/s32-ld.c
BOOTSTRAP = ../stage13/s12cc.s32x ../stage02/s32-as.s32x ../stage07/s32-ld.s32x

.PHONY: all tools verify clean

all: s12cc.s32x tools

# Build the compiler executable
s12cc.s32x: $(COMPILER_SRCS) $(LIBC_SRCS) $(RUNTIME_SRCS) $(BOOTSTRAP)
	@echo "Building s12cc.s32x (stage14, bootstrapped from stage13)..."
	@bash build-s12cc.sh

# Build assembler, archiver, linker
tools: s32-as.s32x s32-ar.s32x s32-ld.s32x

s32-as.s32x s32-ar.s32x s32-ld.s32x: $(TOOL_SRCS) $(LIBC_SRCS) $(RUNTIME_SRCS) $(BOOTSTRAP)
	@echo "Building stage14 tools (s32-as, s32-ar, s32-ld)..."
	@bash build-tools.sh

verify: s12cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x
	@echo "--- Stage 14: full toolchain tests ---"
	@bash run-tests.sh

clean:
	rm -f s12cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x
