identification division.
program-id. p-std2014-dyn-constrec.
*> A dynamic-capacity table in a CONSTANT RECORD (2023 13.18.38.3 rule 33).
data division.
working-storage section.
01 g constant record. 05 t pic x occurs dynamic capacity in c.
procedure division.
    display c
    goback.
