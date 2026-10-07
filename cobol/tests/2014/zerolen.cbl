*> Zero-length items (COBOL 2014; 2023 8.3.3 and 8.5.4): a literal with
*> nothing between its delimiters has length zero -- DISPLAY transfers
*> nothing for it (14.9.11 GR 1), MOVE takes it as SPACE (14.9.25.4 GR 2),
*> a comparison pads it (8.8.4.2), TRIM of spaces returns one; and under
*> >>REF-MOD-ZERO-LENGTH ON (2023 7.3.23) a reference modification's
*> length may be zero, written or computed, the part then an operand that
*> INSPECT, STRING, UNSTRING, MOVE, DISPLAY and the relation conditions
*> treat as the text says (14.9.22, 14.9.43, 14.9.48, 14.9.25, 8.8.4.2).
*> GnuCOBOL 4 takes "" as one SPACE (docs/oracles.md); the directive it
*> agrees on for alphanumeric items.  docs/conformance/lexical.md,
*> docs/conformance/refmod.md
identification division.
program-id. zerolen.
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 r pic x(8).
01 n pic 9(3) value 0.
01 m pic 9(3) value 4.
01 i pic 9(3) value 0.
01 c pic 9(3).
01 cnt pic 9(2).
01 p1 pic x(3).
01 p2 pic x(3).
01 t.
   05 e pic x(2) occurs 3 value "qq".
procedure division.
*> the literal
    display "[" "" "]"
    display "length " function length("")
    display "[" function trim("") "]"
    move "" to x display "[" x "]"
    if x = "" display "equal to spaces" end-if
    move "" to r display "[" r "]"
    display "[" "" & "ab" & "" "]"
    move "abcde" to x
*> the directive: a computed zero, a table element's (the written zero
*> is in zerolen2, which GnuCOBOL refuses)
    >>ref-mod-zero-length on
    display "[" x(3:n) "][" e(2)(1:n) "]"
    display "length " function length(x(2:n))
    display "[" x(m:m - 4) "][" x(m:m - 2) "]"
    perform varying i from 0 by 1 until i > 3
       display "[" x(2:i) "]" with no advancing
    end-perform
    display ""
*> the part as an operand
    move x(2:n) to r display "[" r "]"
    move "wxyz" to x(5:n) display x
    move x(2:3) to x(1:n) display x
    if x(1:n) = "" display "two zero-length operands are equal" end-if
    if x(1:n) = space display "and equal to spaces" end-if
    if x(1:n) < "a" display "and before a" end-if
    move 0 to c
    inspect x(1:n) tallying c for all "a"
    inspect x(1:n) replacing all "a" by "z"
    display "tally " c " " x
    move all "*" to r
    string x(1:2) x(3:n) x(5:1) delimited by size into r display "[" r "]"
    move all "*" to r
    string "ab" delimited by x(1:n) "e" delimited by size into r display "[" r "]"
    move spaces to p1 p2 move 0 to cnt
    unstring x(1:n) delimited by "c" into p1 p2 tallying in cnt
    display "[" p1 "][" p2 "] " cnt
    unstring x delimited by x(1:n) or "d" into p1 p2
    display "[" p1 "][" p2 "]"
    display "upper [" function upper-case(x(1:n)) "] reverse [" function reverse(x(1:n)) "]"
    >>ref-mod-zero-length off
    display "off [" x(2:m) "]"
    stop run.
