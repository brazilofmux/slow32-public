identification division.
program-id. p-partial-literal-subject.
*> A partial expression against a literal subject: the subject goes to the
*> object's left, and a literal is not an identifier (2023 14.9.13.3 rule
*> 6e).
data division.
working-storage section.
01 n pic 9 value 1.
procedure division.
    evaluate 5
        when > n display "x"
    end-evaluate
    goback.
