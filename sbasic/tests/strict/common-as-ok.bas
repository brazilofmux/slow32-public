REM COMMON SHARED arrays after: a record one and an INTEGER one DIMmed with their AS again, a fixed one REDIMmed without
DECLARE SUB mark ()
TYPE spot
  x AS INTEGER
END TYPE
COMMON SHARED pts() AS spot, cnt() AS INTEGER, tags() AS STRING * 2
DIM pts(4) AS spot, cnt(2) AS INTEGER
REDIM tags(3)
mark
PRINT pts(1).x; cnt(2); tags(1); "|"
SUB mark
pts(1).x = 3: cnt(2) = 5: tags(1) = "abc"
END SUB
