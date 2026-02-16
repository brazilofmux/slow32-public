\ Load the assembler and run it on test1.s
: RUN-TEST
    S" selfhost/stage01/test1.s" S" /tmp/test1_fixed.s32o" ASSEMBLE ;
RUN-TEST
BYE
