identification division.
program-id. trimtwo.
*> Each character to delete is one character (2023 15.96.3 rule 2).
data division.
working-storage section.
01 s pic x(4) value "abab".
procedure division.
    display function trim(s "ab")
    stop run.
