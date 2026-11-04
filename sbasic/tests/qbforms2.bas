' more of QuickBASIC's forms, as QBasic 1.1 runs them: GET # and PUT #
' of an array's element and of a record (field by field, a STRING * n
' its n bytes), MID$ = on an element and a field, a FUNCTION f% named
' f (inside it too), a SUB called mid-line before a colon (no label),
' RESTORE in a SUB naming the main program's label, PMAP
DECLARE FUNCTION twice% (n%)
DECLARE SUB note ()
DECLARE SUB rewind ()
TYPE entry
  code AS STRING * 4
  qty AS INTEGER
  cost AS DOUBLE
END TYPE
SCREEN 7
p1 = PMAP(10.6, 0): p2 = PMAP(2.5, 1): p3 = PMAP(-3.5, 2)
SCREEN 0: WIDTH 80
DIM w%(3), r%(3), e AS entry, back AS entry, names$(2)
w%(1) = 513: w%(2) = -2
e.code = "AB": e.qty = 7: e.cost = 1.25
OPEN "QBFORMS.TMP" FOR BINARY AS #1
PUT #1, , w%(1)
PUT #1, , w%(2)
PUT #1, , e
GET #1, 1, r%(3)
GET #1, , r%(2)
GET #1, , back
CLOSE #1
KILL "QBFORMS.TMP"
PRINT r%(3); r%(2); "["; back.code; "]"; back.qty; back.cost
names$(1) = "abcdef": MID$(names$(1), 2, 3) = "XYZW"
e.code = "wxyz": MID$(e.code, 3) = "!!!"
PRINT names$(1); " "; e.code
PRINT twice(5); twice%(6)
x = 1: note: x = 2: note
rewind
READ v: PRINT v
PRINT p1; p2; p3
END
stock:
DATA 11, 22, 33

SUB note
  SHARED x
  PRINT "note"; x
END SUB

SUB rewind
  RESTORE stock
END SUB

FUNCTION twice% (n%)
  twice = n% * 2
END FUNCTION
