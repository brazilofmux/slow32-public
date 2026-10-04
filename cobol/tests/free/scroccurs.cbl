*> OCCURS on a screen VALUE item: each occurrence is placed as though it
*> had the same LINE and COLUMN clauses (2023 13.18.38.4 rule 6), so
*> LINE PLUS 1 puts each one a line below the one before.  ACAS's IRS
*> (cobol ISSUES-124) rules out a sixteen-line entry grid this way.
*> The ANSI stream is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. scroccurs.
data division.
working-storage section.
screen section.
01  grid.
    03  value "HEAD"          line 1 col 1.
    03  occurs 3
        value "[   ] [ ]"     line plus 1 col 2.
    03  value "TAIL"          line plus 1 col 1.
procedure division.
    display grid
    stop run.
