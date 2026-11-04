REM a string passed to a numeric parameter: QB says Type mismatch
DECLARE SUB tally (n)
tally "three"
SUB tally (n)
PRINT n
END SUB
