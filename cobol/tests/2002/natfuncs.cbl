identification division.
program-id. natfuncs.
*> NATIONAL-OF, DISPLAY-OF and CHAR-NATIONAL (COBOL 2002 15.66, 15.26,
*> 15.16; cobol ISSUES-64): the first functions whose result length is
*> known only when they run -- "café" is four bytes of alphanumeric
*> argument but becomes four national characters; "日本" in a PIC X(8)
*> is six bytes and two spaces, and becomes four.  LENGTH counts national
*> characters, BYTE-LENGTH bytes.  A
*> substitution character replaces what does not convert; without one,
*> checking on, the statement raises EC-DATA-CONVERSION when it is done.
*> The bytes are checked through a PIC X redefinition.  No oracle
*> (docs/national.md).
data division.
working-storage section.
01  a        pic x(8).
01  n        pic n(6).
01  nx redefines n pic x(12).
01  s        pic n value n"?".
01  lone     pic n(3).
01  lonex redefines lone pic x(6).
01  bad      pic x(4).
01  u        pic x(9).
01  k        pic 9(5) value 66.
procedure division.
declaratives.
dc section.
    use after exception condition ec-data-conversion.
d1.
    display "  declarative: " function exception-status.
end declaratives.
main section.
m1.
    move function national-of("café") to n
    display "national-of: [" n "] " function length(function national-of("café"))
        " " function byte-length(function national-of("café"))
    move "日本" to a
    display "lengths: " function length(function national-of(a))
        " " function byte-length(function national-of(a))
    move function national-of("AB") to n
    if nx(1:4) = x"00410042" display "bytes: UTF-16BE" end-if
    if function national-of("AB") = n"AB" display "compare: equal" end-if
    move function display-of(n"日本語") to u
    display "display-of: [" u "] " function byte-length(function display-of(n"日本語"))
    move function char-national(k) to n
    display "char-national(66): [" n "]"
    move function char-national(12354) to n
    display "char-national(12354): [" n "]"
    move "ab" to bad  move x"FF" to bad(3:1)  move "c" to bad(4:1)
    move function national-of(bad, s) to n
    display "substituted: [" n "]"
    move function national-of(bad) to n
    display "unchecked: [" n "]"
    move n"xyz" to lone
    move x"D800" to lonex(3:2)
    display "lone, substituted: [" function display-of(lone, "#") "]"
>>TURN EC-DATA-CONVERSION CHECKING ON
    move function national-of(bad) to n
    display "checked: [" n "]"
    move function national-of(bad, s) to n
    display "checked, substituted: [" n "]"
    display "lone, checked: [" function display-of(lone) "]"
    stop run.
