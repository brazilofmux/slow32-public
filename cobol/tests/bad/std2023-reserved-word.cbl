identification division.
program-id. p-std2023-reserved-word.
*> the words 2023 reserved cannot name things (E.2 item 25).
data division.
working-storage section.
01 location pic x(3) value "abc".
procedure division.
    display location.
    stop run.
