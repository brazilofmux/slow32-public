\ Link test3.s32o into a standalone executable
\ Prerequisite: assemble test3.s first with Stage 2 assembler
LINK-INIT
S" /tmp/test3-forth.s32o" LINK-OBJ
S" /tmp/test3-forth-linked.s32x" LINK-EMIT
BYE
