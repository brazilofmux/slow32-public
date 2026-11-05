' A value is held only to the range of what it is stored in: past a
' SINGLE's and into a DOUBLE it goes whole, past a DOUBLE's is an Overflow
ON ERROR GOTO caught
big = 2E+38: small = 3E-38: ten = 10
wide# = big / small: PRINT wide#; seen
wide# = big * ten: PRINT wide#; seen
wide# = big + big + big: PRINT wide#; seen
wide# = -(big * ten): PRINT wide#; seen
wide# = big ^ 2: PRINT wide#; seen
DIM arr#(2)
arr#(1) = big * ten: PRINT arr#(1); seen
huge# = 1D+308: kept# = 5
kept# = huge# * 1.5: PRINT kept#; seen
kept# = 5
narrow = 7
narrow = big * ten: PRINT "single"; seen: seen = 0
PRINT big * ten > 1; seen
PRINT "printed"; big * ten: PRINT seen: seen = 0
kept# = huge# * ten: PRINT "double"; seen: seen = 0
arr#(2) = huge# * ten: PRINT "element"; seen
END
caught:
seen = ERR
RESUME NEXT
