identification division.
program-id. p-fn-no-function-word.
*> An intrinsic function invoked without the word FUNCTION and without a
*> REPOSITORY entry (2023 8.4.3.2.3 rule 2).
data division.
working-storage section.
01 x pic x(5) value "ab".
procedure division.
    display upper-case(x)
    goback.
