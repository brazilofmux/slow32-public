# Stage 02: Subset-C Assembler (s32-as.c) + Archiver (s32-ar.c) + Linker (s32-ld.c)
# Compiled by Stage 01 cc.fth, replaces Stage 01 Forth assembler, archiver, and linker.
# After this stage, Forth tools (stage01) are no longer needed.

PIPELINE = ./run-pipeline.sh
TEST = test3

.PHONY: all test verify clean

all: s32-as.s32x s32-ar.s32x s32-ld.s32x

# Build the assembler executable
s32-as.s32x: s32-as.c libc/start.c libc/stdio.c libc/string_extra.c libc/convert.c crt0.s mmio_no_start.s
	@echo "Building s32-as.s32x..."
	@bash build-s32-as.sh

# Build the archiver executable
s32-ar.s32x: s32-ar.c s32-as.s32x libc/start.c libc/stdio.c libc/string_extra.c libc/convert.c crt0.s mmio_no_start.s
	@echo "Building s32-ar.s32x..."
	@bash build-s32-ar.sh

# Build the linker executable
s32-ld.s32x: s32-ld.c s32-as.s32x libc/start.c libc/stdio.c libc/string_extra.c libc/convert.c crt0.s mmio_no_start.s
	@echo "Building s32-ld.s32x..."
	@bash build-s32-ld.sh

# Baseline: all-Forth pipeline smoke test
test: s32-as.s32x
	@echo "--- Stage 02: baseline (all-Forth) ---"
	@$(PIPELINE) --mode baseline --test $(TEST)

# Progressive: C assembler replaces Forth assembler
verify: s32-as.s32x s32-ar.s32x s32-ld.s32x
	@echo "--- Stage 02: progressive-as (C assembler) ---"
	@$(PIPELINE) --mode progressive-as --test $(TEST)
	@echo "--- Stage 02: archive smoke tests ---"
	@$(PIPELINE) --mode stage6-ar-smoke --test $(TEST)
	@$(PIPELINE) --mode stage6-ar-rc-smoke --test $(TEST)
	@$(PIPELINE) --mode stage6-ar-tx-smoke --test $(TEST)
	@$(PIPELINE) --mode stage6-ar-d-smoke --test $(TEST)
	@echo "--- Stage 02: linker smoke test ---"
	@bash run-spike-ld.sh

clean:
	rm -f s32-as.s32x s32-ar.s32x s32-ld.s32x
