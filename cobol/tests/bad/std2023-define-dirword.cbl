>>DEFINE PUSH AS 1
identification division.
program-id. p-std2023-define-dirword.
*> a 2023 compiler-directive word is no compilation variable (7.3.11.3 rule 1; E.2 item 5).
data division.
working-storage section.
01 x pic x.
procedure division.
    display "x".
    stop run.
