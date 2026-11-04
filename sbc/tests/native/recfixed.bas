' arrays of records and fixed-length strings, compiled: element fields
' read, assigned, swapped and read by INPUT #; records copied; a
' STRING * n cut or padded wherever it is stored; the value assigned
' evaluated before the target's subscripts -- as QBasic 1.1 runs it
DECLARE FUNCTION pick% (k%)
DECLARE SUB stretch (t$)
TYPE crate
  tag AS STRING * 5
  qty AS INTEGER
  wt AS DOUBLE
END TYPE
DIM SHARED shelf(1 TO 3) AS crate
DIM spare AS crate
DIM codes(2) AS STRING * 3
DIM one AS STRING * 4
FOR i = 1 TO 3
  DIM again(2) AS crate
  again(i - 1).qty = again(i - 1).qty + i
  shelf(i).tag = CHR$(64 + i) + "-box-long"
  shelf(i).qty = i * 7
  shelf(i).wt = i / 4
NEXT
PRINT again(0).qty; again(1).qty; again(2).qty
FOR i = 1 TO 3: PRINT "["; shelf(i).tag; "]"; shelf(i).qty; shelf(i).wt: NEXT
SWAP shelf(1), shelf(3)
SWAP shelf(1).qty, shelf(2).qty
spare = shelf(2)
shelf(2).tag = "z"
PRINT "["; spare.tag; "]"; spare.qty; "["; shelf(2).tag; "]"
shelf(pick%(3)).qty = pick%(9)
PRINT
PRINT shelf(3).qty
codes(1) = "abcdef": codes(2) = "q"
PRINT ASC(codes(0)); LEN(codes(0)); "["; codes(1); "]["; codes(2); "]"
CALL stretch(codes(2)): PRINT "["; codes(2); "]"
one = "wx": CALL stretch(one): PRINT "["; one; "]"
SWAP one, codes(1): PRINT "["; one; "]["; codes(1); "]"
OPEN "RECFIXED.TMP" FOR OUTPUT AS #1
PRINT #1, "wide-label-text"
PRINT #1, 41
CLOSE #1
OPEN "RECFIXED.TMP" FOR INPUT AS #1
INPUT #1, shelf(1).tag, shelf(1).qty
CLOSE #1
KILL "RECFIXED.TMP"
PRINT "["; shelf(1).tag; "]"; shelf(1).qty
n = 2
REDIM spot(n) AS crate
spot(n).qty = 12: spot(1) = spot(n)
PRINT spot(1).qty; ASC(spot(0).tag)

FUNCTION pick% (k%)
  PRINT "p"; k%; shelf(2).qty;
  pick% = k%
END FUNCTION

SUB stretch (t$)
  t$ = t$ + "+tail"
  PRINT LEN(t$);
END SUB
