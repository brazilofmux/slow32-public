identification division.
program-id. national.
*> National data, part one (COBOL 2002; cobol ISSUES-62): PICTURE N,
*> national literals N"..." (UTF-8 source) and NX"..." (hex code units),
*> VALUE, MOVE into national items from national, alphanumeric (read as
*> UTF-8), numeric and figurative sources, JUSTIFIED, ALL, comparisons
*> (national against national, alphanumeric and figuratives: HIGH-VALUE
*> is U+FFFF), FUNCTION LENGTH (characters) and BYTE-LENGTH (bytes),
*> INITIALIZE.  A national character is a UTF-16 code unit, stored big-
*> endian; DISPLAY writes UTF-8.  No oracle: GnuCOBOL 4's national data is
*> unfinished (it stores N"é" as two widened bytes and moves without
*> conversion).
data division.
working-storage section.
01  n        pic n(6) value n"Aé€".
01  nx       redefines n pic x(12).
01  m        pic n(4).
01  j        pic n(5) justified right.
01  h        pic n(2) value nx"00480069".
01  a        pic x(8) value "naïve".
01  k        pic 9(3) value 42.
01  l        pic 99.
01  rec.
    05 code-a pic x(3) value "abc".
    05 name-n pic n(4) value n"Zoë".
procedure division.
main.
    display "literal: [" n"Grüße" "]"
    display "n: [" n "]  h from NX: [" h "]"
    move nx(1:4) to a
    if a(1:4) = x"004100E9" display "stored big-endian: 0041 00E9" end-if
    move "naïve" to a
    move n to m                   display "n to m: [" m "]"
    move a to m                   display "alnum to m: [" m "]"
    move k to m                   display "numeric to m: [" m "]"
    move spaces to m              display "spaces: [" m "]"
    move all n"ab" to m           display "all: [" m "]"
    move n"xy" to j               display "justified: [" j "]"
    if n = n"Aé€" display "n = its literal" end-if
    if m = "abab" display "m = alphanumeric abab" end-if
    if n < m display "n < m (A before a)" end-if
    move high-values to m
    if m = high-values display "m = high-values" end-if
    if m > n"zzzz" display "high-values sorts above zzzz" end-if
    move function length(n) to l       display "length " l
    move function byte-length(n) to l  display "byte-length " l
    move function length(n"abc") to l  display "length of a literal " l
    initialize rec
    display "initialized: [" code-a "] [" name-n "]"
    initialize name-n replacing national data by n"Ω"
    display "replacing national: [" name-n "]"
    stop run.
end program national.
