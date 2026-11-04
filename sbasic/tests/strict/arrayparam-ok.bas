' an array parameter, name(), indexed in its SUB: QBasic loads it
DECLARE SUB fill (cells%(), n%)
DIM grid%(5)
CALL fill(grid%(), 3)
PRINT grid%(3)
SUB fill (cells%(), n%)
  cells%(n%) = n% * 4
END SUB
