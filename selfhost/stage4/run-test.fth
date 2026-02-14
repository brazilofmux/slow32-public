\ Compile a test C file to assembly
: RUN-TEST ( addr u addr2 u2 -- )
    COMPILE-FILE ;

: TEST4 S" selfhost/stage4/tests/test4.c" S" /tmp/test4.s" RUN-TEST ;
TEST4
BYE