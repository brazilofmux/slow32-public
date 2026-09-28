identification division.
program-id. natconv.
*> Alphanumeric text becoming national is UTF-8 (cobol ISSUES-63; the
*> user's ruling): a byte that begins no valid UTF-8 sequence is malformed
*> data, not Latin-1 -- it becomes U+FFFD, and with checking on the MOVE
*> raises EC-DATA-CONVERSION (2023 14.9.25 general rule 6; nonfatal: the
*> declarative, then on).  X"E9" alone is Latin-1's e-acute, not UTF-8.
*> No oracle (docs/national.md).
data division.
working-storage section.
01  a        pic x(4).
01  n        pic n(4).
procedure division.
declaratives.
dc section.
    use after exception condition ec-data-conversion.
d1.
    display "  declarative: " function exception-status.
end declaratives.
main section.
m1.
    move "caf" to a  move x"E9" to a(4:1)
    move a to n
    display "unchecked: [" n "]"
>>TURN EC-DATA-CONVERSION CHECKING ON
    move "café" to n
    display "valid UTF-8: [" n "]"
    move a to n
    display "checked: [" n "]"
    stop run.
end program natconv.
