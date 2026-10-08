identification division.
program-id. p-std2014-dyn-cap-receive.
*> The CAPACITY item is not a receiving operand but for SET (2023 13.18.38.3 rule 32).
data division.
working-storage section.
01 g. 05 t pic x occurs dynamic capacity in c.
procedure division.
    move 5 to c
    goback.
