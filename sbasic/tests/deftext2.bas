' DEFtype is the text's (QuickBASIC's), procedures as QuickBASIC saves
' them: a DEFINT after the main program's code types the SUBs after it,
' not the code before it; one inside a SUB goes on into the next; DEFSTR
' makes s the same as s$
DECLARE SUB s1 ()
DECLARE SUB s2 ()
DECLARE SUB s3 (n%)
x = 2.7: PRINT x
CALL s1: CALL s2: CALL s3(4.6)
END
DEFINT A-Z
SUB s1
  y = 2.7: PRINT y
END SUB
SUB s2
  DEFSTR S
  s = "hi": PRINT s; s$
END SUB
SUB s3 (n)
  t = n / 2: PRINT n; t; s$
END SUB
