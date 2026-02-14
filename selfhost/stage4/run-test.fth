\ Compile a test C file to assembly
: RUN-TEST ( addr u addr2 u2 -- )
    COMPILE-FILE ;

: TEST7 S" selfhost/stage4/tests/test7.c" S" /tmp/test7.s" RUN-TEST ;
TEST7
BYE
