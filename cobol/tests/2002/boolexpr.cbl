identification division.
program-id. boolexpr.
*> Boolean expressions (2023 8.8.2, 14.9.8 format 2; cobol ISSUES-77):
*> Table A.2's operations, the Annex D.10 examples, precedence -- B-NOT,
*> B-AND, B-XOR, B-OR, left to right -- a shift taking the precedence of
*> the operation before it (rule 7b), unequal lengths extended with
*> zeros (rule 9), a COMPUTE storing into several receivers by the MOVE
*> rules, a USAGE NATIONAL operand, and boolean expressions in
*> conditions, the simple boolean condition among them.  An ALL literal
*> takes the length of the operand it meets (rules 4, 5).
*> No oracle (docs/boolean.md).
data division.
working-storage section.
01  a        pic 1(4) value b"1100".
01  b        pic 1(4) value b"0101".
01  r        pic 1(4).
01  r6       pic 1(6).
01  r2       pic 1(2).
01  my-flag   pic 1111 value b"0000".
01  my-flag-2 pic 1111.
01  nb       pic 1(4) usage national value b"0011".
01  k        pic 9 value 3.
01  p        pic 1 value b"1".
01  q        pic 1 value b"0".
procedure division.
main.
    compute r = a b-and b    display "and:   " r
    compute r = a b-or b     display "or:    " r
    compute r = a b-xor b    display "xor:   " r
    compute r = b-not a      display "not:   " r
    compute r = a b-shift-l 3   display "sl 3:  " r
    compute r = a b-shift-r 3   display "sr 3:  " r
    compute r = a b-shift-lc 3  display "slc 3: " r
    compute r = a b-shift-rc k  display "src k: " r
    move b"0011" to my-flag-2
    compute my-flag = b-not my-flag-2        display "D.10 not:   " my-flag
    compute my-flag = my-flag b-shift-l 2    display "D.10 sl 2:  " my-flag
    move b"1100" to my-flag
    compute my-flag = my-flag b-shift-rc 3   display "D.10 src 3: " my-flag
    compute my-flag-2 = my-flag-2 b-or bx"8" display "D.10 or 8:  " my-flag-2
    compute r = a b-or b b-and b"0000"       display "or before and: " r
    compute r = (a b-or b) b-and b"0110"     display "parenthesized: " r
    compute r = a b-xor b b-or b"0001"       display "xor, then or:  " r
    compute r = b"0001" b-or a b-shift-l 1   display "shift at or's precedence: " r
    compute r6 = a b-or b"11"                display "unequal lengths: " r6
    compute r6 r2 = a b-xor nb               display "two receivers: " r6 " " r2
    compute r = a b-xor all b"10"           display "xor all 10: " r
    compute r6 = a b-and all b"1"           display "and all 1 (4 positions, stored in 6): " r6
    if (a b-or all b"01") = b"1101" display "condition: a b-or all 01 = 1101" end-if
    if a b-and b = b"0100" display "condition: a b-and b = 0100" end-if
    if b-not a not = b"0010" display "condition: b-not a not = 0010" end-if
    if p b-and q display "p and q" else display "not (p and q)" end-if
    if p b-or q display "p or q" end-if
    if not (p b-xor p) display "not (p xor p)" end-if
    stop run.
