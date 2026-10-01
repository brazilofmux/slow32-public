*> PIC X(8) COMP-5 (Micro Focus; docs/usage.md): eight bytes in the
*> machine's order, unsigned, holding what they hold -- 2^64 - 1, twenty
*> digits -- the same item as BINARY-DOUBLE UNSIGNED, so on the wide path
*> (-std=2002).  It was "not implemented (up to seven bytes)" until
*> abrignoli_COBSOFT's twelve programs needed it (ISSUES 120).  The oracle
*> runs in GnuCOBOL's default dialect.
identification division.
program-id. comp5x8.
data division.
working-storage section.
01 big   pic x(8) comp-5.
01 big-bytes redefines big.
   05 b  pic x occurs 8.
01 small pic x(8) comp-5 value 1000000.
01 o1    pic 999.
01 o2    pic 999.
01 o8    pic 999.
procedure division.
    move 18446744073709551615 to big
    display "max " big
    move 258 to big
    display "258 " big
    compute o1 = function ord(b(1)) - 1
    compute o2 = function ord(b(2)) - 1
    compute o8 = function ord(b(8)) - 1
    display "low bytes " o1 " " o2 " " o8
    compute big = small * small * 1000
    display "10^15 " big
    add 999 to big
    display "plus 999 " big
    subtract small from big
    display "less 10^6 " big
    if big > small display "big > small" end-if
    move big to small
    display "moved " small
    stop run.
