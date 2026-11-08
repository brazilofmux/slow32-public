# Build the tzlookup CLI for the SLOW-32 target.
#
# Reuses the shared header (../src) and the generated data tables (../gen) so
# the 101 KB of tables are never duplicated. Only the lookup logic and CLI are
# ported to plain C (see tzlookup.c / tz_cli.c).

SLOW32  ?= $(HOME)/slow-32
LLVM    ?= $(HOME)/llvm-project/build/bin
CLANG    = $(LLVM)/clang
LLC      = $(LLVM)/llc
AS       = $(SLOW32)/tools/assembler/slow32asm
LD       = $(SLOW32)/tools/linker/s32-ld
EMU      = $(SLOW32)/tools/emulator/slow32
RT       = $(SLOW32)/runtime

TARGET   = slow32-unknown-none
CFLAGS   = -target $(TARGET) -S -emit-llvm -O2 -I../src -I../gen -I$(RT)/include
LLCFLAGS = -mtriple=$(TARGET)

# libc_mmio gives us argv + stdio over the MMIO ring buffers; --mmio carves out
# the MMIO region the emulator uses for args/console I/O.
LIBS     = $(RT)/libc_mmio.s32a $(RT)/libs32.s32a
CRT0     = $(RT)/crt0.s32o
LDFLAGS  = --mmio 64K

.PHONY: all test clean

all: tz.s32x tz_test.s32x

%.s32o: %.c
	$(CLANG) $(CFLAGS) $< -o $*.ll
	$(LLC) $(LLCFLAGS) $*.ll -o $*.s.raw
	python3 chunk_long_strings.py $*.s.raw $*.s
	$(AS) $*.s $@

tz.s32x: tz_cli.s32o tzlookup.s32o
	$(LD) $(LDFLAGS) -o $@ $(CRT0) $^ $(LIBS)

tz_test.s32x: test_tzlookup.s32o tzlookup.s32o
	$(LD) $(LDFLAGS) -o $@ $(CRT0) $^ $(LIBS)

# Run the on-emulator assertion suite.
test: tz_test.s32x
	$(EMU) tz_test.s32x

# Distill the CLI artifact into the central ~/s32x build repo.
S32X ?= $(HOME)/s32x
dist: tz.s32x
	cp tz.s32x $(S32X)/tz.s32x
	@echo "copied tz.s32x -> $(S32X)/tz.s32x"

clean:
	rm -f *.ll *.s *.s.raw *.s32o *.s32x
