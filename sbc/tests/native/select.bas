' SELECT CASE, EXIT, SWAP in s32-bc
FOR I = 0 TO 12
  SELECT CASE I
  CASE 1, 3, 5: PRINT "odd";
  CASE 2 TO 4: PRINT "2-4";
  CASE IS > 10, IS = 0: PRINT "big/0";
  CASE ELSE: PRINT "else";
  END SELECT
NEXT: PRINT
FOR I% = 1 TO 4
  SELECT CASE I% * 1.5
  CASE IS < 3: PRINT "lt3";
  CASE 4.5: PRINT "four.5";
  END SELECT
NEXT: PRINT
FOR K = 1 TO 4
  READ W$
  SELECT CASE W$
  CASE "apple": PRINT "A";
  CASE "b" TO "c": PRINT "B-C";
  CASE IS >= "x": PRINT "X+";
  CASE ELSE: PRINT "?";
  END SELECT
NEXT: PRINT
DATA apple, banana, zebra, mango
X$ = "same"
SELECT CASE X$ + ""
CASE "same": PRINT "matched"
END SELECT
SELECT CASE 7
CASE 1: PRINT "no"
END SELECT
PRINT "after"
N = 0
FOR I = 1 TO 100
  N = N + 1
  IF I = 5 THEN EXIT FOR
NEXT: PRINT N; I
N = 0
DO
  N = N + 1
  IF N > 6 THEN EXIT DO
LOOP: PRINT N
FOR I = 1 TO 3
  DO WHILE 1
    J = J + 1: IF J MOD 2 = 0 THEN EXIT DO
  LOOP
  IF I = 2 THEN EXIT FOR
NEXT: PRINT I; J
A = 1: B = 2: SWAP A, B: PRINT A; B
A$ = "x": B$ = "yy": SWAP A$, B$: PRINT A$; B$; LEN(A$)
DIM C%(3), D#(2): C%(1) = 5: C%(2) = 6: SWAP C%(1), C%(2): PRINT C%(1); C%(2)
D#(1) = 1 / 3: E# = 2: SWAP D#(1), E#: PRINT D#(1); E#
F(1) = 1.5: G = 2.5: SWAP G, F(1): PRINT F(1); G
S$(1) = "p": S$(2) = "q": SWAP S$(1), S$(2): PRINT S$(1); S$(2)
