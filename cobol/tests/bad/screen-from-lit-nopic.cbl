identification division.
program-id. fromnopic.
*> FROM literal-1 needs the entry's PICTURE (2002 13.15.2 rule 7).
data division.
screen section.
01 s.
   05 line 1 col 1 from "-".
procedure division.
    stop run.
