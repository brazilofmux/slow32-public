*> INSPECT's general rules at their edges (X3.23-1985 VI-89..; 2023
*> 14.9.22.4): one pass left to right, the first phrase that matches takes
*> the positions -- overlapping patterns, competing phrases, LEADING, FIRST,
*> BEFORE and AFTER together, CHARACTERS in a range, CONVERTING after a
*> delimiter, TALLYING then REPLACING.  docs/conformance/string.md
identification division.
program-id. inspectrules.
data division.
working-storage section.
01 x  pic x(12).
01 t1 pic 99.
01 t2 pic 99.
01 t3 pic 99.
procedure division.
    move "aaaa" to x  move 0 to t1
    inspect x tallying t1 for all "aa"
    display "1 overlap: " t1
    move "abcabc" to x  move 0 to t1 t2
    inspect x tallying t1 for all "bc" t2 for all "b"
    display "2 order: " t1 " " t2
    move "aabaa" to x  move 0 to t1
    inspect x tallying t1 for leading "a"
    display "3 leading: " t1
    move "xxaxxa" to x
    inspect x replacing first "a" by "Z"
    display "4 first: " x
    move "a.b.c.d" to x  move 0 to t1
    inspect x tallying t1 for all "." after initial "a" before initial "d"
    display "5 range: " t1
    move "abcdefgh" to x
    inspect x replacing characters by "*" after initial "b" before initial "g"
    display "6 chars: " x
    move "hello world" to x
    inspect x converting "lo" to "LO" after initial " "
    display "7 conv: " x
    move "abab" to x  move 0 to t1
    inspect x tallying t1 for all "a" replacing all "a" by "c"
    display "8 both: " t1 " " x
    move "aaa  aaa" to x  move 0 to t1
    inspect x tallying t1 for characters before initial space
    display "9 chars before: " t1
    move "ab" to x  move 0 to t1 t2 t3
    inspect x tallying t1 for all "a" t2 for characters t3 for all "b"
    display "10 chars mix: " t1 " " t2 " " t3
    stop run.
