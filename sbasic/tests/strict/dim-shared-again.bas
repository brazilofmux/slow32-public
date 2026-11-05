REM a DIM in a SUB of an array DIM SHARED made: QB says Array already dimensioned
DECLARE SUB s ()
DIM SHARED k(4)
s
SUB s
DIM k(2)
END SUB
