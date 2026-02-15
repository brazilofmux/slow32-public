\ Compile a test C file to assembly
: RUN-TEST ( addr u addr2 u2 -- )
    COMPILE-FILE ;

: DUMP S" /tmp/test_bisect1.c" S" /tmp/test_bisect1.s" RUN-TEST ;
DUMP
BYE
