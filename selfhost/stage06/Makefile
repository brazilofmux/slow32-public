# Stage 06: Subset-C Archiver (s32-ar.c)
# Compiled by Stage 04 cc.fth, assembled by Stage 05 s32-as.s32x,
# linked by Stage 03 link.fth. Replaces Stage 02 Forth archiver.

PIPELINE = ../stage05/run-pipeline.sh
TEST = test3

.PHONY: all verify clean

all: s32-ar.s32x

# Build the archiver executable
s32-ar.s32x: s32-ar.c ../stage05/s32-as.s32x ../stage05/libc/start.c ../stage05/libc/stdio.c ../stage05/libc/string_extra.c ../stage05/libc/convert.c ../stage05/crt0.s ../stage05/mmio_no_start.s
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
