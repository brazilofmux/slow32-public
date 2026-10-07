*> Structured constants (COBOL 2014; 2023 13.18.15 CONSTANT RECORD, D.21):
*> a level 01 record whose content is its initial state for good -- the
*> VALUEs given, and for the rest what INITIALIZE WITH FILLER ALL TO VALUE
*> THEN TO DEFAULT gives (GR 1): zeros in numeric items, spaces in the
*> others.  Its items are sending operands anywhere: DISPLAY, COMPUTE,
*> MOVE and MOVE CORRESPONDING, reference modification, a condition-name,
*> a table under it and a REDEFINES inside it (the D.21 example), STRING,
*> INSPECT TALLYING, a function argument, BY CONTENT, a GLOBAL one from a
*> contained program, one in LOCAL-STORAGE (static all the same, 8.6.4),
*> and the record whole.  The storage is read-only: the receiving uses are
*> refused (tests/bad/std2014-constrec-*), a store that gets past them --
*> through a pointer, a called program's BY REFERENCE argument -- is a
*> memory fault.  No oracle: GnuCOBOL 4 does not take the clause.
*> docs/conformance/data-division.md
identification division.
program-id. constrec.
data division.
working-storage section.
01 a-data-item constant record.
   02 field-1 binary picture s9(9).
   02 field-2 display picture x(10).
   02 array-init pic x(26) value "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
   02 display-chars redefines array-init.
      03 display-char picture x occurs 26 times.
   02 filler pic x(3) value all "Q".
   02 filler pic x(4).
01 rates global constant record.
   02 rate-count pic 99 value 3.
   02 rate-table.
      03 rate-row occurs 3 times indexed by rx.
         04 rate-code pic x(2) value "ab".
         04 rate-pct pic s9(3)v99 value -12.50.
   02 status-flag pic x value "y".
      88 status-on value "y".
      88 status-off value "n".
   02 note pic x(12) value "hello, world".
01 g.
   02 rate-count pic 99.
   02 note pic x(12).
01 w pic x(20).
01 n pic s9(5)v99.
01 i pic 99.
01 cnt pic 99.
procedure division.
    display "[" a-data-item(5:) "]"
    display field-1 " [" field-2 "] " display-char(3) display-char(26)
    display rate-count of rates " " rate-pct(2) " " status-flag " " note of rates
    compute n = rate-pct(1) * 2 + rate-count of rates display n
    move array-init(5:3) to w display w
    if status-on display "status on" end-if
    if not status-off display "and not off" end-if
    perform varying i from 1 by 1 until i > rate-count of rates display display-char(i) with no advancing end-perform
    display ""
    move corresponding rates to g display "[" g "]"
    move all "*" to w
    string note of rates delimited by "," rate-code(3) delimited by size into w display "[" w "]"
    move 0 to cnt
    inspect array-init tallying cnt for all "A" "E" "I" "O" "U" display "vowels " cnt
    display function length(a-data-item) " " function lower-case(note of rates) " " function trim(a-data-item(37:))
    set rx to 2 display rate-code(rx)
    call "constrec-sub" using by content field-2 by reference w
    display "[" w "]"
    call "constrec-local"
    call "constrec-local"
    stop run.
identification division.
program-id. constrec-sub.
data division.
linkage section.
01 p1 pic x(10).
01 p2 pic x(20).
procedure division using p1 p2.
    move p1 to p2
    move rate-count of rates to p2(12:2)
    display "sub sees " note of rates
    goback.
end program constrec-sub.
identification division.
program-id. constrec-local.
data division.
working-storage section.
01 calls pic 9 value 0.
local-storage section.
01 lc constant record.
   02 lf1 pic x(5) value "local".
   02 lf2 pic 9(3).
   02 lf3 pic s9(4) binary.
   02 lf4 pic x(2).
procedure division.
    add 1 to calls
    display "call " calls ": [" lf1 lf2 "|" lf4 "] " lf2 " " lf3 " " function length(lc)
    goback.
end program constrec-local.
end program constrec.
