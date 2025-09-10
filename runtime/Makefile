# SLOW-32 Runtime Library Makefile

# Paths
LLVM_BIN = ~/llvm-project/build/bin
ASSEMBLER = ../tools/assembler/slow32asm
ARCHIVER = ../tools/utilities/s32-ar

# Compiler flags
CLANG = $(LLVM_BIN)/clang
LLC = $(LLVM_BIN)/llc
TARGET = slow32-unknown-none
# Default optimization level (can be overridden per-file)
OPT_LEVEL = -O2
CFLAGS_BASE = -target $(TARGET) -S -emit-llvm -Iinclude
CFLAGS = $(CFLAGS_BASE) $(OPT_LEVEL)

# Core runtime objects (always needed)
CORE_OBJS = crt0.s32o intrinsics.s32o intrinsics_extra.s32o \
            debug_char.s32o yield.s32o

# Standard library objects for libc.s32a
LIBC_OBJS = string.s32o string_extra.s32o \
            stdio.s32o stdio_minimal.s32o printf.s32o putchar.s32o \
            malloc.s32o stdlib.s32o stdlib_extra.s32o stdlib_utils.s32o \
            convert_extra.s32o ctype.s32o memory_extra.s32o

# Math library objects for libm.s32a (future - currently disabled due to float support)
# LIBM_OBJS = math.s32o

# Builtins for 64-bit operations
BUILTIN_OBJS = builtins.s32o

# All object files
ALL_OBJS = $(CORE_OBJS) $(LIBC_OBJS) $(BUILTIN_OBJS)

# C source files that need compilation
C_SOURCES = printf.c builtins.c string.c malloc.c stdlib_utils.c \
            stdio.c stdlib_extra.c string_extra.c ctype.c \
            stdio_minimal.c convert_extra.c memory_extra.c
# Note: math.c excluded due to floating point operations not yet supported

# Default target builds everything
all: tools $(ALL_OBJS) libc.s32a libs32.s32a

# Ensure tools are built first
tools:
	@if [ ! -f $(ASSEMBLER) ]; then \
		echo "Building assembler..."; \
		$(MAKE) -C ../tools/assembler; \
	fi
	@if [ ! -f $(ARCHIVER) ]; then \
		echo "Building archiver..."; \
		$(MAKE) -C ../tools/utilities s32-ar; \
	fi

# Pattern rule for .s -> .s32o
%.s32o: %.s
	$(ASSEMBLER) $< $@

# Generic pattern rule for C -> LLVM IR -> ASM -> OBJ
%.ll: %.c
	$(CLANG) $(CFLAGS) $< -o $@

%.s: %.ll
	$(LLC) -mtriple=$(TARGET) $< -o $@

# Files that might need -O1 instead of -O2 (if register pressure is an issue)
# Override optimization for specific problematic files
stdio.ll: OPT_LEVEL = -O1
stdio.ll: stdio.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

# Archive targets
libc.s32a: $(LIBC_OBJS)
	@echo "Creating libc.s32a..."
	$(ARCHIVER) c $@ $^
	@echo "Archive size: $$(ls -lh $@ | awk '{print $$5}')"

libs32.s32a: $(CORE_OBJS)
	@echo "Creating libs32.s32a..."
	$(ARCHIVER) c $@ $^
	@echo "Archive size: $$(ls -lh $@ | awk '{print $$5}')"

# Special handling for assembly-only files that don't have C sources
convert_extra.s32o: convert_extra.s
	$(ASSEMBLER) $< $@

memory_extra.s32o: memory_extra.s
	$(ASSEMBLER) $< $@

stdio_minimal.s32o: stdio_minimal.s
	$(ASSEMBLER) $< $@

# Clean targets
clean:
	rm -f *.s32o *.ll *.s32a
	@echo "Cleaned runtime directory"

clean-intermediate:
	rm -f *.ll
	@echo "Cleaned intermediate files"

# Rebuild from C sources
rebuild: clean all

# Show sizes of archives
size:
	@echo "Archive sizes:"
	@ls -lh *.s32a 2>/dev/null || echo "No archives built yet"
	@echo ""
	@echo "Object file sizes:"
	@ls -lh *.s32o 2>/dev/null | head -20 || echo "No object files built yet"

# Help target
help:
	@echo "SLOW-32 Runtime Library Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  all              - Build all libraries and archives (default)"
	@echo "  libc.s32a        - Build C standard library archive"
	@echo "  libs32.s32a      - Build core runtime archive"
	@echo "  clean            - Remove all built files"
	@echo "  clean-intermediate - Remove intermediate .ll files only"
	@echo "  rebuild          - Clean and rebuild everything"
	@echo "  size             - Show sizes of built archives and objects"
	@echo "  help             - Show this help message"
	@echo ""
	@echo "Individual objects can be built with: make <name>.s32o"
	@echo "Example: make printf.s32o"

.PHONY: all clean clean-intermediate rebuild size help tools
# Archive size comparison:
# Original (with optimizations): 22.8KB
# New (optimizations disabled): 31KB
# Increase: ~36% due to disabled BranchFolder and MachineBlockPlacement passes
