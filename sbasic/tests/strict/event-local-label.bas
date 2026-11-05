REM a trap's label is the main program's: QB says Label not defined
DECLARE SUB s ()
s
SYSTEM
SUB s
ON TIMER(1) GOSUB t
EXIT SUB
t: RETURN
END SUB
