\ Compile a test C file to assembly
: RUN-TEST ( addr u addr2 u2 -- )
    COMPILE-FILE ;

: TEST9 S" selfhost/stage4/tests/test9.c" S" /tmp/test9.s" RUN-TEST ;
TEST9
BYE
