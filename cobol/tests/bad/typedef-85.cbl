identification division.
program-id. td85.
*> TYPEDEF is COBOL 2002; under -std=85 it is refused.
data division.
working-storage section.
01  money-t pic 9(5) typedef.
procedure division.
    stop run.
