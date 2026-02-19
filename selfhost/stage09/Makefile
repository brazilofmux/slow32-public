# Stage 09: Subset-C Self-Compilation Fixed-Point Proof
# cc-min (stage08) compiles itself → Gen2. No Forth tools.
# Verify: Gen2 compiles cc-min → Gen3, gen2.s == gen3.s (fixed point).

.PHONY: all verify clean

all: cc-min.s32x

# Build the self-compiled compiler (Gen2)
cc-min.s32x: ../stage08/cc-min.s32x ../stage08/cc-min.c ../stage08/cc-min-pass1.c ../stage08/cc-min-pass2.c ../stage08/cc-min-pass3.c ../stage02/s32-as.s32x ../stage02/s32-ld.s32x ../stage02/libc/start.c ../stage02/libc/stdio.c ../stage02/libc/string_extra.c ../stage02/libc/convert.c ../stage02/crt0.s ../stage02/mmio_no_start.s
	@echo "Building cc-min.s32x (Gen2: self-compiled)..."
	@bash build-cc-min-gen2.sh

verify: cc-min.s32x
	@echo "--- Stage 09: fixed-point proof ---"
	@bash run-spike.sh

clean:
	rm -f cc-min.s32x
