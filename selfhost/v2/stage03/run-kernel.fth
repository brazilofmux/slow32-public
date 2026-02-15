\ Link the Forth kernel from object files and archives
\ Prerequisite: assemble kernel.s first with Stage 2 assembler
LINK-INIT
S" runtime/crt0.s32o" LINK-OBJ
S" forth/kernel.s32o" LINK-OBJ
65536 LINK-MMIO
S" runtime/libc_mmio.s32a" LINK-ARCHIVE
S" runtime/libs32.s32a" LINK-ARCHIVE
S" /tmp/kernel-forth-linked.s32x" LINK-EMIT
BYE
