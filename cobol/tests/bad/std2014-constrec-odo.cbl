identification division.
program-id. p-constrec-odo.
*> No OCCURS DEPENDING ON inside a structured constant (2023 13.18.38.3 rules 19, 23).
data division.
working-storage section.
01 n pic 9 value 2.
01 c constant record.
   02 f1 pic x(5) value "abcde".
   02 g occurs 1 to 3 depending on n.
      03 x pic x.
procedure division.
    display f1
    stop run.
