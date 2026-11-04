' records as parameters, compiled: a record variable and an element of
' an array of records passed by reference to a SUB and a FUNCTION (x AS
' a TYPE), its fields read and changed there and seen by the caller,
' and passed on from one procedure to another
TYPE box
  w AS INTEGER
  tag AS STRING * 3
  mass AS DOUBLE
END TYPE
DECLARE SUB grow (b AS box, by%)
DECLARE SUB relay (b AS box)
DECLARE FUNCTION area& (b AS box)
DIM one AS box, many(2) AS box
one.w = 6: one.tag = "ab": one.mass = 1.5
grow one, 4
PRINT one.w; one.tag; "|"; one.mass; area&(one)
many(1).w = 3: many(1).tag = "xyz"
relay many(1)
PRINT many(1).w; many(1).tag; area&(many(1))
END
SUB grow (b AS box, by%)
b.w = b.w + by%
b.tag = b.tag + "c"
b.mass = b.mass * 2
END SUB
SUB relay (b AS box)
grow b, 10
b.tag = "rel"
END SUB
FUNCTION area& (b AS box)
area& = CLNG(b.w) * b.w
END FUNCTION
