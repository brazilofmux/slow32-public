*> A cut never parts a surrogate pair (docs/national.md, the owner's ruling
*> of 2026-10-09): a MOVE that truncates, on the right or (JUSTIFIED) on
*> the left, a national-edited receiver whose N positions are interrupted,
*> and a STRING that overflows each drop a pair that does not fit whole --
*> keeping one half would be corruption, not a shorter representation.
*> The text's code-unit positions would leave a lone surrogate (8.5.1.4):
*> a documented deviation. A lone surrogate already in the data moves as
*> it is. U+1F600 is two positions. No oracle: GnuCOBOL has no UTF-16.
identification division.
program-id. nattrunc.
data division.
working-storage section.
01 n3 pic n(3).
01 n4 pic n(4).
01 j3 pic n(3) justified right.
01 e1 pic nbn.
01 e2 pic nnbn.
01 s4 pic n(4).
01 p pic 99.
procedure division.
    move n"ab😀" to n3
    display "right cut [" n3 "] length " function length(n3)
    move n"ab😀" to n4
    display "fits [" n4 "]"
    move n"😀ab" to j3
    display "justified cut [" j3 "]"
    move n"😀cd" to j3
    display "justified, the low half cut [" j3 "]"
    move n"x😀" to n3
    display "pair exactly at the end [" n3 "]"
    move n"😀" to e1
    display "edited NBN [" e1 "]"
    move n"a😀" to e2
    display "edited NNBN [" e2 "]"
    move n"😀b" to e2
    display "edited pair in NN [" e2 "]"
    move all n"-" to s4
    move 1 to p
    string n"abc😀" delimited by size into s4 with pointer p
        on overflow display "overflow, pointer " p
    end-string
    display "string [" s4 "]"
    move all n"-" to s4
    move 1 to p
    string n"ab😀" delimited by size into s4 with pointer p
        on overflow display "unexpected overflow"
    end-string
    display "string fits [" s4 "] pointer " p
    move n"x😀" to n4
    move n4(1:2) to n3
    display "a part naming one half: the lone surrogate moves as it is [" n3 "]"
    stop run.
