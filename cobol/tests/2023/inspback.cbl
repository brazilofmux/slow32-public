*> INSPECT BACKWARD (2023 14.9.22.4 rule 3): the scan from the right,
*> the BEFORE and AFTER boundaries found in that direction (the text's
*> own example: TALLYING CHARACTERS BEFORE "12" in "A12C21D12EF" gives 2),
*> matching leftmost-first at each position (note 2), the positions a
*> match takes never reused, LEADING and FIRST from the right, CONVERTING
*> with a range; and the logical operator XOR / EXCLUSIVE-OR (8.7.6,
*> 8.8.4.9): true when one included condition is, between AND and OR in
*> the order of evaluation (8.8.4.13).  No oracle: GnuCOBOL 4 has neither.
*> docs/conformance/edition-2023.md
identification division.
program-id. inspback.
data division.
working-storage section.
01 s pic x(11) value "A12C21D12EF".
01 t pic x(11).
01 n pic 99.
01 m pic 99.
01 one pic 9 value 1.
01 two pic 9 value 2.
procedure division.
    move 0 to n inspect backward s tallying n for characters before "12" display "before 12: " n
    move 0 to n inspect s tallying n for characters before "12" display "forward: " n
    move 0 to n inspect backward s tallying n for characters after "12" display "after 12: " n
    move 0 to n inspect backward s tallying n for all "12" display "all 12: " n
    move "AAAAB" to t move 0 to n inspect backward t tallying n for all "AA" display "AA in AAAAB: " n
    move "AAAAB" to t move 0 to n inspect t tallying n for all "AA" display "forward: " n
    move "AAABAA" to t move 0 to n inspect backward t(1:6) tallying n for leading "A" display "leading A backward: " n
    move "AAABAA" to t move 0 to n inspect t tallying n for leading "A" display "forward: " n
    move "xAxBxC" to t inspect backward t replacing first "x" by "*" display t
    move "xAxBxC" to t inspect t replacing first "x" by "*" display t
    move "A12C21D12EF" to t inspect backward t replacing characters by "." before "12" display t
    move "A12C21D12EF" to t inspect backward t converting "ACDEF" to "acdef" after "21" display t
    move "A12C21D12EF" to t inspect backward t converting "ACDEF" to "acdef" before "21" display t
    if one = 1 xor two = 2 display "xor tt" else display "xor tt false" end-if
    if one = 1 xor two = 3 display "xor tf true" end-if
    if one = 2 exclusive-or two = 3 display "x" else display "xor ff false" end-if
    if one = 1 or one = 2 xor one = 1 display "or binds looser: true" end-if
    if one = 1 xor one = 1 and one = 2 display "and binds tighter: true" end-if
    if one = 1 xor one = 2 xor one = 1 display "x" else display "three: false" end-if
    stop run.
