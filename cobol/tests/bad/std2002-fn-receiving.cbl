identification division.
program-id. p-fn-receiving.
*> A function-identifier as a receiving operand (2023 8.4.3.2.3 rule 1).
data division.
working-storage section.
01 x pic x(5).
procedure division.
    move "ab" to function upper-case(x)
    goback.
