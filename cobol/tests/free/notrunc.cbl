*> notrunc -- an unsigned COMP-5 item keeps the binary field's whole
*> capacity, so a four-byte one holds 0 .. 4294967295 and its top bit is
*> value, not sign.  GitHub #28: the hot path's sign fixup read
*> 4000000000 as -294967296 and stored its magnitude, 294967296.
*>
*> Storing: the 85 text's rule is that an unsigned receiver takes the
*> magnitude, on a MOVE and on an arithmetic result alike.  GnuCOBOL
*> agrees on every MOVE (M) and on GIVING with a signed operand, and
*> takes the value modulo 2^32 on an in-place ADD/SUBTRACT/COMPUTE
*> (V, C2, N3, N5: 3 - 5 gives 4294967294) -- N5 and N6 are the same
*> subtraction with and without GIVING and it answers them differently,
*> so that is a fast path showing through, not a rule.  The text's
*> answer is in .expected, GnuCOBOL's in .oracle-expected
*> (docs/oracles.md).  COMP-5 is an implementor usage: default dialect.
identification division.
program-id. notrunc.
data division.
working-storage section.
01  u5  pic 9(9) comp-5 value 0.
01  v5  pic 9(9) comp-5 value 0.
01  s5  pic s9(9) comp-5 value 0.
01  s9  pic s9(9) comp value 0.
01  u2  pic 9(4) comp-5 value 0.
01  w   pic 9(10) value 0.
procedure division.
main-procedure.
*> the issue's case: a hot ADD carrying the value past 2^31
    move 2000000000 to u5
    add 2000000000 to u5
    if u5 > 2000000000 then display 'S ok' else display 'S BAD' end-if
    move u5 to w
    display 'T ' w
*> the same sum on the generic path (a four-byte unsigned operand is never hot)
    move 2000000000 to u5
    move 2000000000 to v5
    add v5 to u5
    move u5 to w
    display 'G1 ' w
    move 2000000000 to u5
    compute u5 = u5 + 2000000000
    move u5 to w
    display 'G2 ' w
*> a literal past 2^31, moved
    move 4000000000 to u5
    move u5 to w
    display 'U ' w
*> a value with the top bit set survives MOVE and orders unsigned
    move 4000000000 to u5
    move u5 to v5
    move v5 to w
    display 'Y ' w
    move 3000000000 to v5
    if u5 > v5 then display 'Z ok' else display 'Z BAD' end-if
    if u5 > 1 then display 'Z2 ok' else display 'Z2 BAD' end-if
    if v5 < u5 then display 'Z3 ok' else display 'Z3 BAD' end-if
*> PERFORM VARYING across 2^31
    move 0 to v5
    perform varying u5 from 2147483000 by 1000 until u5 > 2147485000
        add 1 to v5
    end-perform
    move u5 to w
    display 'P ' w ' ' v5
*> wrapping at 2^32 is the field's capacity, both paths
    move 4294967295 to u5
    add 1 to u5
    move u5 to w
    display 'W ' w
    move 4294967295 to u5
    move 1 to v5
    add v5 to u5
    move u5 to w
    display 'W2 ' w
*> a negative result: the unsigned receiver takes the magnitude
    move 3 to u5
    subtract 5 from u5
    move u5 to w
    display 'V ' w
    move 3 to u5
    move 5 to v5
    subtract v5 from u5
    move u5 to w
    display 'V2 ' w
    move 3 to u5
    move -5 to s9
    add s9 to u5
    move u5 to w
    display 'V3 ' w
    move 5 to u5
    subtract 10 from u5 giving v5
    move v5 to w
    display 'N3 ' w
    move 5 to u5
    move 10 to s9
    subtract s9 from u5
    move u5 to w
    display 'N5 ' w
    move 5 to u5
    move 10 to s9
    subtract s9 from u5 giving u5
    move u5 to w
    display 'N6 ' w
    move 0 to u5
    set u5 down by 1
    move u5 to w
    display 'D ' w
*> a negative value moved
    move -5 to u5
    move u5 to w
    display 'M1 ' w
    move -5 to s5
    move s5 to u5
    move u5 to w
    display 'M3 ' w
*> two bytes: the sign is genuine in a word, so nothing changes there
    move 65535 to u2
    add 1 to u2
    display 'A2 ' u2
    move 40000 to u2
    add 30000 to u2
    display 'B2 ' u2
    move 3 to u2
    subtract 5 from u2
    display 'C2 ' u2
    stop run.
end program notrunc.
