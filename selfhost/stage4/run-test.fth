\ Compile a test C file to assembly
: RUN-TEST ( addr u addr2 u2 -- )
    COMPILE-FILE ;

: TEST1 S" selfhost/stage4/tests/test1.c" S" /tmp/test1.s" RUN-TEST ;
: TEST2 S" selfhost/stage4/tests/test2.c" S" /tmp/test2.s" RUN-TEST ;
: TEST3 S" selfhost/stage4/tests/test3.c" S" /tmp/test3.s" RUN-TEST ;

TEST1 TEST2 TEST3
BYE
