# Stage 05: Complete compiler toolchain (compiler + assembler + archiver + linker + runtime)
# All C sources compiled by Stage 04 compiler, assembled by Stage 04 s32-as.s32x,
# linked by Stage 04 s32-ld.s32x.

COMPILER_SRCS = s12cc.c c_lexer_gen.c ast.h parser.h sema.h optimize.h codegen.h pp.h
LIBC_SRCS = libc/start.c libc/stdio.c libc/string_extra.c libc/string_more.c libc/ctype.c libc/convert.c libc/malloc.c
RUNTIME_SRCS = crt0.s mmio_no_start.s
TOOL_SRCS = tools/s32-as.c tools/s32_formats_min.h tools/s32-ar.c tools/s32ar_min.h tools/s32-ld.c tools/s32util_min.h tools/slow32dump.c tools/slow32dis.c
BOOTSTRAP_CC = ../stage04/cc.s32x
BOOTSTRAP = $(BOOTSTRAP_CC) ../stage04/s32-as.s32x ../stage04/s32-ld.s32x

.PHONY: all tools verify clean

all: cc.s32x tools

# Build the compiler executable
cc.s32x: $(COMPILER_SRCS) $(LIBC_SRCS) $(RUNTIME_SRCS) $(BOOTSTRAP)
	@echo "Building cc.s32x (stage05, bootstrapped from stage04)..."
	@bash build-s12cc.sh

# Build assembler, archiver, linker, dump, disassembler
tools: s32-as.s32x s32-ar.s32x s32-ld.s32x slow32dump.s32x slow32dis.s32x

s32-as.s32x s32-ar.s32x s32-ld.s32x slow32dump.s32x slow32dis.s32x: $(TOOL_SRCS) $(LIBC_SRCS) $(RUNTIME_SRCS) $(BOOTSTRAP)
	@echo "Building stage05 tools (s32-as, s32-ar, s32-ld, slow32dump, slow32dis)..."
	@bash build-tools.sh

verify: cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x slow32dump.s32x slow32dis.s32x
	@echo "--- Stage 05: full toolchain tests ---"
	@bash run-tests.sh

clean:
	rm -f cc.s32x s32-as.s32x s32-ar.s32x s32-ld.s32x slow32dump.s32x slow32dis.s32x
