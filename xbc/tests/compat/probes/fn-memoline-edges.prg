* Functions: MLCOUNT() and MEMOLINE() at their edges -- trailing blanks,
* a word longer than the width, leading blanks, a hard return, the empty
* string (0 lines), and a line past the last (blanks, the width wide)
LOCAL a := {"London      ", "Ada            ", "Dennis         ", "Murray Hill ", "one two three four", "abc  def", "  lead", "x" + CHR(13) + CHR(10) + "y", ""}
LOCAL i, w, n, k
FOR i := 1 TO LEN(a)
   FOR w := 5 TO 8 STEP 3
      n := MLCOUNT(a[i], w, 4, .T.)
      ?? "[" + STRTRAN(STRTRAN(a[i], CHR(13), "\r"), CHR(10), "\n") + "] w" + LTRIM(STR(w)) + " n" + LTRIM(STR(n)) + ":"
      FOR k := 1 TO n + 1
         ?? " <" + MEMOLINE(a[i], w, k, 4, .T.) + ">"
      NEXT
      ?
   NEXT
NEXT
