\ Link the Forth kernel from minimal selfhost sources
\ Prerequisite: assemble these with Stage 1 assembler first:
\   - selfhost/v2/stage01/crt0_minimal.s -> crt0_minimal.s32o
\   - selfhost/v2/stage01/mmio_minimal.s -> mmio_minimal.s32o
\   - forth/kernel.s -> kernel.s32o
\
\ No host-built archives required - mmio_minimal.s32o provides all
\ runtime functions needed by kernel.s (open, close, read, write, etc.)

LINK-INIT
S" selfhost/v2/stage01/crt0_minimal.s32o" LINK-OBJ
S" forth/kernel.s32o" LINK-OBJ
S" selfhost/v2/stage01/mmio_minimal.s32o" LINK-OBJ
65536 LINK-MMIO
S" /tmp/kernel-selfhost.s32x" LINK-EMIT
BYE
