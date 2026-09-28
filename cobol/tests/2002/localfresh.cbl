identification division.
program-id. localfresh.
*> LOCAL-STORAGE in a program that is not RECURSIVE (COBOL 2002; cobol
*> ISSUES-49): a fresh copy, set to its VALUEs, on every CALL, where
*> WORKING-STORAGE keeps its last-used state (2023 14.6.2.3).
procedure division.
main.
    call "counter"
    call "counter"
    call "counter"
    stop run.
end program localfresh.

identification division.
program-id. counter.
data division.
working-storage section.
01  kept     pic 9 value 0.
local-storage section.
01  fresh    pic 9 value 0.
01  grp.
    05 tag   pic x(3) value "abc".
    05 num   pic 99 value 7.
procedure division.
c1.
    add 1 to kept
    add 1 to fresh
    add 1 to num
    display "kept " kept " fresh " fresh " " tag " " num
    move "xyz" to tag
    exit program.
end program counter.
