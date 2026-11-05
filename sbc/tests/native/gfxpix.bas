' LINE, its style, its clipping, PAINT with colors and tiles, CIRCLE and DRAW:
' each picture's hash against what QBasic 1.1 answered for the same statements
DEFINT A-Z
DECLARE SUB hash (n%, c&)

bad = 0
SCREEN 13
LINE (-30,41)-(68,-14),3,B,&HE513
hash n%, c&
IF n% <> 30 OR c& <> 950217 THEN PRINT "op 0"; n%; c&: bad = bad + 1
SCREEN 13
LINE (93,67)-(86,85),3,B,&H9627
hash n%, c&
IF n% <> 30 OR c& <> 950217 THEN PRINT "op 1"; n%; c&: bad = bad + 1
SCREEN 13
LINE (52,21)-(-60,-20),3
hash n%, c&
IF n% <> 83 OR c& <> 330481 THEN PRINT "op 2"; n%; c&: bad = bad + 1
SCREEN 13
LINE (-35,75)-(-58,85),3,,&H35D2
hash n%, c&
IF n% <> 83 OR c& <> 330481 THEN PRINT "op 3"; n%; c&: bad = bad + 1
SCREEN 13
LINE (41,24)-(29,51),3,B,&HFFEA
hash n%, c&
IF n% <> 147 OR c& <> 1048326 THEN PRINT "op 4"; n%; c&: bad = bad + 1
SCREEN 13
LINE (71,-21)-(27,-18),3,,&H4AC9
hash n%, c&
IF n% <> 147 OR c& <> 1048326 THEN PRINT "op 5"; n%; c&: bad = bad + 1
SCREEN 13
LINE (13,77)-(-24,39),3
hash n%, c&
IF n% <> 147 OR c& <> 1048326 THEN PRINT "op 6"; n%; c&: bad = bad + 1
SCREEN 13
LINE (34,53)-(58,68),3
hash n%, c&
IF n% <> 159 OR c& <> 563279 THEN PRINT "op 7"; n%; c&: bad = bad + 1
SCREEN 13
LINE (89,87)-(-31,68),3,,&HC00A
hash n%, c&
IF n% <> 159 OR c& <> 563279 THEN PRINT "op 8"; n%; c&: bad = bad + 1
SCREEN 13
LINE (93,85)-(41,26),3,B,&H9063
hash n%, c&
IF n% <> 173 OR c& <> 739564 THEN PRINT "op 9"; n%; c&: bad = bad + 1
SCREEN 13
LINE (62,-34)-(87,17),3,B,&HD172
hash n%, c&
IF n% <> 173 OR c& <> 739564 THEN PRINT "op 10"; n%; c&: bad = bad + 1
SCREEN 13
LINE (-48,-14)-(99,69),3
hash n%, c&
IF n% <> 231 OR c& <> 449887 THEN PRINT "op 11"; n%; c&: bad = bad + 1
SCREEN 13
LINE (63,18)-(-29,76),3,,&HB6EB
hash n%, c&
IF n% <> 270 OR c& <> 705287 THEN PRINT "op 12"; n%; c&: bad = bad + 1
SCREEN 13
LINE (79,24)-(83,62),3,,&H4EC1
hash n%, c&
IF n% <> 270 OR c& <> 705287 THEN PRINT "op 13"; n%; c&: bad = bad + 1
SCREEN 13
LINE (80,88)-(118,82),3,B,&H66E7
hash n%, c&
IF n% <> 270 OR c& <> 705287 THEN PRINT "op 14"; n%; c&: bad = bad + 1
SCREEN 13
LINE (50,11)-(15,28),3
hash n%, c&
IF n% <> 304 OR c& <> 560773 THEN PRINT "op 15"; n%; c&: bad = bad + 1
SCREEN 13
LINE (-22,77)-(73,85),3
hash n%, c&
IF n% <> 304 OR c& <> 560773 THEN PRINT "op 16"; n%; c&: bad = bad + 1
SCREEN 13
LINE (-9,26)-(117,84),3
hash n%, c&
IF n% <> 361 OR c& <> 583211 THEN PRINT "op 17"; n%; c&: bad = bad + 1
SCREEN 13
LINE (67,71)-(82,-32),3,B,&H5111
hash n%, c&
IF n% <> 361 OR c& <> 583211 THEN PRINT "op 18"; n%; c&: bad = bad + 1
SCREEN 13
LINE (57,-15)-(52,26),3
hash n%, c&
IF n% <> 388 OR c& <> 1037888 THEN PRINT "op 19"; n%; c&: bad = bad + 1
SCREEN 13
LINE (-14,68)-(62,-24),3,,&HE73D
hash n%, c&
IF n% <> 421 OR c& <> 635217 THEN PRINT "op 20"; n%; c&: bad = bad + 1
SCREEN 13
LINE (67,54)-(-36,9),3,,&H694C
hash n%, c&
IF n% <> 446 OR c& <> 467833 THEN PRINT "op 21"; n%; c&: bad = bad + 1
SCREEN 13
LINE (-23,75)-(-57,-17),3
hash n%, c&
IF n% <> 446 OR c& <> 467833 THEN PRINT "op 22"; n%; c&: bad = bad + 1
SCREEN 13
LINE (88,-19)-(96,10),3
hash n%, c&
IF n% <> 446 OR c& <> 467833 THEN PRINT "op 23"; n%; c&: bad = bad + 1
SCREEN 7
LINE (2,2)-(40,30),15,B
LINE (9,9)-(20,14),4,BF
PAINT (5,5),CHR$(&HAA)+CHR$(&H55)+CHR$(0)+CHR$(0),15
hash n%, c&
IF n% <> 1131 OR c& <> 429307 THEN PRINT "op 24"; n%; c&: bad = bad + 1
SCREEN 7
LINE (2,2)-(40,30),15,B
LINE (3,10)-(39,10),1
PAINT (5,5),CHR$(255)+CHR$(0)+CHR$(0)+CHR$(0)+CHR$(0)+CHR$(255)+CHR$(0)+CHR$(0),15
hash n%, c&
IF n% <> 1131 OR c& <> 1007551 THEN PRINT "op 25"; n%; c&: bad = bad + 1
SCREEN 13
LINE (2,2)-(40,30),15,B
LINE (3,10)-(39,10),6
PAINT (5,5),CHR$(6)+CHR$(4),15
hash n%, c&
IF n% <> 428 OR c& <> 350210 THEN PRINT "op 26"; n%; c&: bad = bad + 1
SCREEN 13
LINE (2,2)-(40,30),15,B
LINE (3,10)-(39,10),6
PAINT (5,5),CHR$(6)+CHR$(4),15,CHR$(6)
hash n%, c&
IF n% <> 428 OR c& <> 350210 THEN PRINT "op 27"; n%; c&: bad = bad + 1
SCREEN 13
LINE (2,2)-(40,30),15,B
LINE (15,3)-(15,29),5
PAINT (5,5),5,15
hash n%, c&
IF n% <> 1131 OR c& <> 59140 THEN PRINT "op 28"; n%; c&: bad = bad + 1
SCREEN 13
LINE (2,2)-(40,30),5,B
LINE (9,9)-(12,12),3,BF
PAINT (5,5),5
hash n%, c&
IF n% <> 1131 OR c& <> 292484 THEN PRINT "op 29"; n%; c&: bad = bad + 1
SCREEN 13
LINE (2,2)-(40,30),9,B
PAINT (5,5),CHR$(1)+CHR$(2)
hash n%, c&
IF n% <> 3721 OR c& <> 842867 THEN PRINT "op 30"; n%; c&: bad = bad + 1
SCREEN 13
LINE (2,2)-(40,30),15,B
COLOR 9
PAINT (5,5),CHR$(1)+CHR$(2)
COLOR 15
hash n%, c&
IF n% <> 3721 OR c& <> 842867 THEN PRINT "op 31"; n%; c&: bad = bad + 1
SCREEN 1
LINE (2,2)-(40,30),3,B
PAINT (5,5),CHR$(&H1B)+CHR$(&HE4),3
hash n%, c&
IF n% <> 874 OR c& <> 811880 THEN PRINT "op 32"; n%; c&: bad = bad + 1
SCREEN 2
LINE (2,2)-(40,30),1,B
PAINT (5,5),CHR$(&HF0)+CHR$(&H0F),1
hash n%, c&
IF n% <> 633 OR c& <> 992434 THEN PRINT "op 33"; n%; c&: bad = bad + 1
SCREEN 13
VIEW (5,5)-(50,40),,9
LINE (2,2)-(30,20),15,B
PAINT (5,5),CHR$(1)+CHR$(2),15
VIEW
hash n%, c&
IF n% <> 719 OR c& <> 649032 THEN PRINT "op 34"; n%; c&: bad = bad + 1
SCREEN 13
CIRCLE (30,30),20,3,1,4
CIRCLE (30,30),15,3,-1,-4,.7
hash n%, c&
IF n% <> 776 OR c& <> 383174 THEN PRINT "op 35"; n%; c&: bad = bad + 1
SCREEN 2
CIRCLE (30,30),12,1
CIRCLE (30,30),20,1
hash n%, c&
IF n% <> 152 OR c& <> 61342 THEN PRINT "op 36"; n%; c&: bad = bad + 1
SCREEN 7
CIRCLE (30,30),12,4
hash n%, c&
IF n% <> 64 OR c& <> 548576 THEN PRINT "op 37"; n%; c&: bad = bad + 1
SCREEN 13
DRAW "C3 BM20,20 M +7,+3 M -7,+13 TA30 U20"
hash n%, c&
IF n% <> 38 OR c& <> 491962 THEN PRINT "op 38"; n%; c&: bad = bad + 1
SCREEN 0
PRINT "differ from QBasic:"; bad
SYSTEM

SUB hash (n%, c&)
n% = 0: c& = 0
FOR y% = 0 TO 60: FOR x% = 0 TO 60
p% = POINT(x%, y%)
IF p% > 0 THEN c& = (c& * 7 + x% * 3 + y% * 5 + p%) AND &HFFFFF: n% = n% + 1
NEXT x%: NEXT y%
END SUB
