' RETURN label in a SUB: QBasic will not load it (Illegal in procedure
' or DEF FN)
hop
SUB hop
  GOSUB there
there:
  RETURN away
away:
  PRINT "away"
END SUB
