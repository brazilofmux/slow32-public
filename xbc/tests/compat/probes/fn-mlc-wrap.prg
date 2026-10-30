* Functions: MLCOUNT(), MLCTOPOS(), MPOSTOLC() and MEMOLINE() with lWrap
* .T. and .F. -- with word wrap off a line ends only at a hard return
LOCAL c := "the quick brown fox jumps" + CHR(13) + CHR(10) + "over the lazy dog" + CHR(13) + CHR(10) + "end"
LOCAL a, w
FOR w := 1 TO 2
   ? IIF(w == 1, "wrap", "nowrap"), MLCOUNT(c, 10, 4, w == 1)
   ? MLCTOPOS(c, 10, 2, 3, 4, w == 1), MLCTOPOS(c, 10, 3, 0, 4, w == 1)
   a := MPOSTOLC(c, 10, 30, 4, w == 1)
   ? a[1], a[2]
   a := MPOSTOLC(c, 10, 46, 4, w == 1)
   ? a[1], a[2]
   ? "[" + MEMOLINE(c, 10, 2, 4, w == 1) + "]"
NEXT
