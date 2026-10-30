// Strings past 255 characters and strings holding NULs: the operators and
// the functions over them, binary conversions round-tripping, FREAD of a
// whole file by reference, MEMOREAD / MEMOWRIT.
PROCEDURE Main
   LOCAL a := SPACE(1990), h, buf
   ? "len", LEN(a), LEN(REPLICATE("ab", 300)), LEN(a + "X")
   ? "sub", SUBSTR(SPACE(300) + "XYZ", 301, 2), RIGHT(SPACE(300) + "XYZ", 2), LEN(LEFT(a, 1000))
   ? "nul", LEN(CHR(0) + "a"), ASC(CHR(0)), BIN2W(I2BIN(513)), BIN2L(L2BIN(70000)), LEN(I2BIN(0)), BIN2I(I2BIN(-2))
   ? "cmp", SPACE(300) == SPACE(300), SPACE(300) + "a" == SPACE(300) + "b", "x" $ SPACE(300) + "x", AT("y", SPACE(400) + "y"), RAT("y", "y" + SPACE(400) + "y")
   ? "trim", LEN(TRIM(SPACE(500))), LEN(ALLTRIM(SPACE(300) + "q" + SPACE(300))), LEN(UPPER(REPLICATE("a", 400))), LEN(LTRIM(SPACE(300) + "q"))
   ? "pad", LEN(PADR("a", 600)), LEN(PADL("a", 600)), LEN(PADC("ab", 601)), LEN(STRTRAN(REPLICATE("ab", 200), "a", "xyz")), LEN(STUFF(SPACE(400), 200, 0, SPACE(300)))
   ? "empty", EMPTY(SPACE(600)), EMPTY(SPACE(600) + "."), LEN(CHR(0) + CHR(0) + CHR(1)), ASC(SUBSTR(CHR(0) + CHR(7), 2, 1))
   h := FCREATE("ls.tmp")
   ? "wrote", FWRITE(h, REPLICATE("0123456789", 199)), FCLOSE(h)
   h := FOPEN("ls.tmp")
   buf := SPACE(1990)
   ? "read", FREAD(h, @buf, 1990), LEN(buf), SUBSTR(buf, 1985, 5), FCLOSE(h)
   ? "memo", LEN(MEMOREAD("ls.tmp")), MEMOWRIT("ls2.tmp", REPLICATE("z", 1000)), LEN(MEMOREAD("ls2.tmp"))
   ? FERASE("ls.tmp"), FERASE("ls2.tmp")
   ? "print", REPLICATE("-", 300)
   RETURN
