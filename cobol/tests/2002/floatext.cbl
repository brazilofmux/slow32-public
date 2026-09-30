*> FLOAT-SHORT, FLOAT-LONG, FLOAT-EXTENDED (2023 13.18.60.4 rule 13):
*> each holds what the one before it holds -- FLOAT-EXTENDED is a double
*> here, as FLOAT-LONG is.  The values go through decimal items, so the
*> float DISPLAY form (MF's here, GnuCOBOL's own) plays no part.
identification division.
program-id. floatext.
data division.
working-storage section.
01 fs  usage float-short value 0.1.
01 fl  usage float-long.
01 fx  usage float-extended.
01 d   pic s9(3)v9(8).
01 w   pic 99.
procedure division.
    move fs to fl
    move fl to fx
    compute d rounded = fx * 3
    display "short via long via extended, times 3: " d
    move 1 to fx
    compute fx = fx / 3
    compute d = fx
    display "one third: " d
    move fx to fl
    if fl = fx display "long holds the extended value" end-if
    *> FLOAT-EXTENDED's size is the implementor's (8 here, 16 in GnuCOBOL)
    compute w = function length(fs) + function length(fl) * 10
    display "lengths of short and long: " w
    stop run.
