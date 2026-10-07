identification division.
program-id. p-std2002-zero-literal.
*> A zero-length literal is COBOL 2014 (8.3.3.2 rule 3; 2002 8.3.1.2.1.2
*> rule 1 says more than zero characters).
data division.
working-storage section.
01 x pic x(5) value "abcde".
01 n pic 9 value 0.

procedure division.
    move "" to x
    stop run.
