identification division.
program-id. natrefmod.
*> Reference modification of national items (2023 8.4.2.4; cobol
*> ISSUES-67): start and length count character positions, two bytes
*> each, and the part is national -- as a sending and as a receiving
*> operand, with literal and computed positions, with the length
*> omitted, in a table element, and checked for EC-BOUND-REF-MOD in
*> characters: position 7 of six is past the end, and the condition is
*> fatal -- its declarative, then the run ends.  No oracle
*> (docs/national.md).
data division.
working-storage section.
01  n        pic n(6) value n"日本語テキス".
01  m        pic n(4).
01  t.
    05 e     pic n(3) occurs 2.
01  k        pic 99 value 2.
01  l        pic 99 value 3.
procedure division.
declaratives.
dc section.
    use after exception condition ec-bound-ref-mod.
d1.
    display "  declarative: " function exception-status(1:16).
end declaratives.
main section.
m1.
    display "literal: [" n(2:3) "] [" n(5:) "]"
    display "computed: [" n(k:l) "] [" n(k + 3:) "]"
    display "length: " function length(n(2:3)) " " function byte-length(n(2:3))
    move n(4:2) to m
    display "sending: [" m "]"
    move "ab" to n(1:2)
    display "receiving alphanumeric: [" n "]"
    move n"語" to n(k:1)
    display "receiving computed: [" n "]"
    move spaces to n(5:2)
    display "a figurative: [" n "]"
    move all n"xy" to n(2:3)
    display "ALL: [" n "]"
    move n"一二三四五六" to t
    display "table: [" e(2)(2:2) "]"
    if n(2:3) = n"xyx" display "compare: equal" end-if
>>TURN EC-BOUND-REF-MOD CHECKING ON
    move 4 to k
    display "in bounds: [" n(k:1) "]"
    move 7 to k
    display "past the end (fatal):"
    move n(k:1) to m
    display "not reached"
    stop run.
