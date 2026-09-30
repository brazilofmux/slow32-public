*> BP-E20: a literal of 161 positions.  X3.23-1985 allows 1 through 160;
*> taken, as the NIST SQL suite's 255-position literals are (yts750).
identification division.
program-id. p.
data division.
working-storage section.
01 i pic x(200) value "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".
procedure division.
    stop run.
