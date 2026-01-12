# SLOW-32 Runtime Library Makefile

# Paths
# Allow LLVM_BIN to be overridden by environment variable (for Docker builds)
LLVM_BIN ?= ~/llvm-project/build/bin
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

# Compiler runtime objects (always needed, linked last)
# These provide compiler support functions like division, intrinsics
COMPILER_RT_OBJS = intrinsics.s32o intrinsics_extra.s32o \
                   builtins.s32o divsi3.s32o \
                   debug_char.s32o

# Core C library objects (common to all libc versions)
LIBC_CORE_OBJS = string.s32o string_extra.s32o \
                 malloc.s32o stdlib.s32o stdlib_extra.s32o stdlib_utils.s32o \
                 convert.s32o convert_extra.s32o ctype.s32o memory_extra.s32o \
                 printf_enhanced.s32o start.s32o yield.s32o \
                 cxxabi.s32o time_util.s32o

# I/O objects for DEBUG version (simple, unbuffered)
STDIO_DEBUG_OBJS = putchar.s32o stdio_minimal.s32o

# I/O objects for MMIO version (buffered, ring buffer)
STDIO_MMIO_OBJS = stdio.s32o stdio_buffered.s32o flush_mmio.s32o

# MMIO-only helpers
LIBC_MMIO_EXTRA_OBJS = mmio_request.s32o time_mmio.s32o \
                       stat_mmio.s32o args_mmio.s32o exit_mmio.s32o \
                       envp_mmio.s32o fs_mmio.s32o dirent_mmio.s32o

# Debug-only extras (fallback stubs)
LIBC_DEBUG_EXTRA_OBJS = stat_stub.s32o args_stub.s32o exit_debug.s32o \
                        envp_stub.s32o fs_stub.s32o dirent_stub.s32o

# Math library objects for libm.s32a (future - currently disabled due to float support)
# LIBM_OBJS = math.s32o

# Startup code (linked first, not in any archive)
STARTUP_OBJ = crt0.s32o

# All object files
ALL_OBJS = $(STARTUP_OBJ) $(COMPILER_RT_OBJS) $(LIBC_CORE_OBJS) \
           $(STDIO_DEBUG_OBJS) $(STDIO_MMIO_OBJS) $(LIBC_MMIO_EXTRA_OBJS) \
           $(LIBC_DEBUG_EXTRA_OBJS)

# C source files that need compilation
C_SOURCES = printf_enhanced.c convert.c builtins.c string.c malloc.c stdlib_utils.c \
            stdio.c stdlib_extra.c string_extra.c ctype.c \
            stdio_minimal.c convert_extra.c memory_extra.c \
            stdio_buffered.c flush_mmio.c flush_debug.c \
            debug_char.c yield.c putchar.c mmio_request.c time_mmio.c \
            stat_mmio.c stat_stub.c start.c args_mmio.c args_stub.c \
            exit_mmio.c exit_debug.c envp_mmio.c envp_stub.c \
            cxxabi.c time_util.c \
            fs_mmio.c fs_stub.c dirent_mmio.c dirent_stub.c
# Note: math.c excluded due to floating point operations not yet supported

# Default target builds everything
all: tools $(ALL_OBJS) libs32.s32a libc_debug.s32a libc_mmio.s32a

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

# Files that need -O2 (lower optimization to avoid issues)
# Override optimization for specific problematic files
stdio.ll: OPT_LEVEL = -O2
stdio.ll: stdio.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

# Enhanced printf with long branch/jump support
printf_enhanced.ll: OPT_LEVEL = -O2
printf_enhanced.ll: printf_enhanced.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

flush_mmio.ll: OPT_LEVEL = -O2
flush_mmio.ll: flush_mmio.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

stdio_buffered.ll: OPT_LEVEL = -O2
stdio_buffered.ll: stdio_buffered.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

string_extra.ll: OPT_LEVEL = -O2
string_extra.ll: string_extra.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

string.ll: OPT_LEVEL = -O2
string.ll: string.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

builtins.ll: OPT_LEVEL = -O2
builtins.ll: builtins.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

malloc.ll: OPT_LEVEL = -O2
malloc.ll: malloc.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

ctype.ll: OPT_LEVEL = -O2
ctype.ll: ctype.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

convert_extra.ll: OPT_LEVEL = -O2
convert_extra.ll: convert_extra.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

convert.ll: OPT_LEVEL = -O0
convert.ll: convert.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

memory_extra.ll: OPT_LEVEL = -O2
memory_extra.ll: memory_extra.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

stdlib_extra.ll: OPT_LEVEL = -O2
stdlib_extra.ll: stdlib_extra.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

# Environment variable support (needs -O0 due to i1 load issue)
envp_mmio.ll: OPT_LEVEL = -O0
envp_mmio.ll: envp_mmio.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

envp_stub.ll: OPT_LEVEL = -O0
envp_stub.ll: envp_stub.c
	$(CLANG) $(CFLAGS_BASE) $(OPT_LEVEL) $< -o $@

# Special LLC rules to avoid double optimization
stdio.s: stdio.ll
	$(LLC) -mtriple=$(TARGET) -O2 $< -o $@

string_extra.s: string_extra.ll
	$(LLC) -mtriple=$(TARGET) -O2 $< -o $@

# Archive targets
# Compiler runtime library (always linked last)
libs32.s32a: $(COMPILER_RT_OBJS)
	@echo "Creating libs32.s32a (compiler runtime)..."
	$(ARCHIVER) c $@ $^
	@echo "Archive size: $$(ls -lh $@ | awk '{print $$5}')"

# Debug version of libc (uses DEBUG instruction for I/O)
libc_debug.s32a: $(LIBC_CORE_OBJS) $(STDIO_DEBUG_OBJS) $(LIBC_DEBUG_EXTRA_OBJS)
	@echo "Creating libc_debug.s32a (DEBUG I/O)..."
	$(ARCHIVER) c $@ $^
	@echo "Archive size: $$(ls -lh $@ | awk '{print $$5}')"

# MMIO version of libc (uses ring buffer for I/O)
libc_mmio.s32a: $(LIBC_CORE_OBJS) $(STDIO_MMIO_OBJS) $(LIBC_MMIO_EXTRA_OBJS)
	@echo "Creating libc_mmio.s32a (MMIO I/O)..."
	$(ARCHIVER) c $@ $^
	@echo "Archive size: $$(ls -lh $@ | awk '{print $$5}')"

# Note: The pattern rule %.s32o: %.s handles all assembly files
# including both hand-written ones and those generated from C sources

# Clean targets
clean:
	rm -f *.s32o *.ll *.s32a
	# Clean generated .s files (but keep hand-written assembly sources)
	@for src in $(C_SOURCES); do \
		base=$$(basename $$src .c); \
		rm -f $$base.s; \
	done
	@echo "Cleaned runtime directory"

clean-intermediate:
	rm -f *.ll
	# Clean generated .s files from C sources
	@for src in $(C_SOURCES); do \
		base=$$(basename $$src .c); \
		rm -f $$base.s; \
	done
	@echo "Cleaned intermediate files"

# Remove old/obsolete archive versions
clean-old:
	@echo "Removing obsolete archive versions..."
	-rm -f libc.s32a libc_fixed.s32a libc_simple.s32a libc_stabilized.s32a
	-rm -f libs32_test.s32a libc_debug_old.s32a libc_mmio_old.s32a
	-rm -f libc_original.s32a libc_debug_broken.s32a libc_mmio_broken.s32a
	@echo "Old archives removed."

# Rebuild from C sources
rebuild: clean all

# Show sizes of archives
size:
	@echo "Archive sizes:"
	@ls -lh *.s32a 2>/dev/null || echo "No archives built yet"
	@echo ""
	@echo "Object file sizes:"
	@ls -lh *.s32o 2>/dev/null | head -20 || echo "No object files built yet"

# Check critical symbols are present
check: libs32.s32a libc_debug.s32a libc_mmio.s32a
	@echo "Checking critical symbols..."
	@echo -n "  __udivsi3 in libs32.s32a: "
	@$(ARCHIVER) -t libs32.s32a | grep -q "divsi3.s32o" && echo "✓" || (echo "✗ MISSING!"; exit 1)
	@echo -n "  __divsi3 in libs32.s32a: "
	@$(ARCHIVER) -t libs32.s32a | grep -q "divsi3.s32o" && echo "✓" || (echo "✗ MISSING!"; exit 1)
	@echo -n "  printf in libc_debug.s32a: "
	@$(ARCHIVER) -t libc_debug.s32a | grep -q "printf_enhanced.s32o" && echo "✓" || (echo "✗ MISSING!"; exit 1)
	@echo -n "  malloc in libc_debug.s32a: "
	@$(ARCHIVER) -t libc_debug.s32a | grep -q "malloc.s32o" && echo "✓" || (echo "✗ MISSING!"; exit 1)
	@echo "All critical symbols present."

# Help target
help:
	@echo "SLOW-32 Runtime Library Build System"
	@echo "====================================="
	@echo ""
	@echo "Libraries produced:"
	@echo "  libs32.s32a      - Compiler runtime (intrinsics, division, builtins)"
	@echo "  libc_debug.s32a  - C library using DEBUG instruction for I/O"
	@echo "  libc_mmio.s32a   - C library using MMIO ring buffer for I/O"
	@echo "  crt0.s32o        - Startup code (not in any archive)"
	@echo ""
	@echo "Targets:"
	@echo "  all              - Build all libraries (default)"
	@echo "  check            - Verify critical symbols are present"
	@echo "  clean            - Remove all built files"
	@echo "  clean-old        - Remove obsolete archive versions"
	@echo "  clean-intermediate - Remove intermediate .ll files only"
	@echo "  rebuild          - Clean and rebuild everything"
	@echo "  size             - Show sizes of built archives and objects"
	@echo "  help             - Show this help message"
	@echo ""
	@echo "Link order: crt0.s32o user.s32o libc_*.s32a libs32.s32a"
	@echo "Example: s32-ld -o prog.s32x crt0.s32o prog.s32o libc_debug.s32a libs32.s32a"

.PHONY: all clean clean-intermediate clean-old rebuild size help tools check
# Archive size comparison:
# Original (with optimizations): 22.8KB
# New (optimizations disabled): 31KB
# Increase: ~36% due to disabled BranchFolder and MachineBlockPlacement passes
