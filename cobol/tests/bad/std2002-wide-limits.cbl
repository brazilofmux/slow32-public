identification division.
program-id. widelim.
*> COBOL 2002's limit is 31 digits (2023 13.18.40.3; 8.3.1.2.2.2 rule 1):
*> a PICTURE or a literal of 32 is refused, and a composite of operands
*> past 31 (14.7.7 rule 2).
data division.
working-storage section.
01 a pic 9(32).
01 b pic 9(31).
01 c pic 9(5).
procedure division.
    move 12345678901234567890123456789012 to c.
    add b 0.1 to c.
    stop run.
