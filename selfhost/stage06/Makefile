# Stage 06: Subset-C Archiver (s32-ar.c)
# Compiled by Stage 04 cc.fth, assembled by Stage 02 s32-as.s32x,
# linked by Stage 03 link.fth. Replaces Stage 02 Forth archiver.

PIPELINE = ../stage02/run-pipeline.sh
TEST = test3

.PHONY: all verify clean

all: s32-ar.s32x

# Build the archiver executable
s32-ar.s32x: s32-ar.c ../stage02/s32-as.s32x ../stage02/libc/start.c ../stage02/libc/stdio.c ../stage02/libc/string_extra.c ../stage02/libc/convert.c ../stage02/crt0.s ../stage02/mmio_no_start.s
	@echo "Building s32-ar.s32x..."
	@bash build-s32-ar.sh

# Smoke test: archive create, list, extract, replace, delete, move
verify: s32-ar.s32x
	@echo "--- Stage 06: archive smoke tests ---"
	@$(PIPELINE) --mode stage6-ar-smoke --test $(TEST)
	@$(PIPELINE) --mode stage6-ar-rc-smoke --test $(TEST)
	@$(PIPELINE) --mode stage6-ar-tx-smoke --test $(TEST)
	@$(PIPELINE) --mode stage6-ar-d-smoke --test $(TEST)

clean:
	rm -f s32-ar.s32x
