* Functions: the smaller ones of the guide's list -- binary conversions,
* memo lines, DESCEND, HARDCR, MEMOTRAN, files, environment
LOCAL s, b
PRIVATE n
? BIN2I(I2BIN(1234)), BIN2I(I2BIN(-2)), BIN2W(I2BIN(-2)), BIN2L(L2BIN(100000)), BIN2L(L2BIN(-7))
s := "the quick brown fox jumps over the lazy dog" + CHR(13) + CHR(10) + "second line here"
? MLCOUNT(s, 20), MLCOUNT(s, 79)
? "[" + MEMOLINE(s, 20, 1) + "]"
? "[" + MEMOLINE(s, 20, 2) + "]"
? "[" + MEMOLINE(s, 20, 3) + "]"
? "[" + MEMOLINE(s, 20, 4) + "]"
? MLPOS(s, 20, 2), MLPOS(s, 20, 3)
? MLCTOPOS(s, 20, 2, 3)
b := MPOSTOLC(s, 20, 25)
? b[1], b[2]
? ASC(HARDCR(CHR(141) + CHR(10) + "x")), MEMOTRAN("a" + CHR(13) + CHR(10) + "b" + CHR(141) + CHR(10) + "c")
? DESCEND(5), DESCEND(-2.5), DESCEND(.T.), ASC(DESCEND("A")), ASC(SUBSTR(DESCEND("AB"), 2))
? ISDIGIT("7x"), ISDIGIT("x7"), ISDIGIT("")
? EXP(0), ROUND(EXP(1), 3), LOG(1), ROUND(LOG(100), 4)
? FKLABEL(1), FKLABEL(12), FKMAX() >= 10
? MEMOWRIT("probe_memo.txt", "hello memo"), MEMOREAD("probe_memo.txt")
? FRENAME("probe_memo.txt", "probe_memo2.txt"), FILE("probe_memo2.txt"), FILE("probe_memo.txt")
? FERASE("probe_memo2.txt"), FILE("probe_memo2.txt"), FERASE("no_such_probe_file.txt")
? ERRORLEVEL(), ERRORLEVEL(3), ERRORLEVEL()
n := 10
b := MEMVARBLOCK("n")
? EVAL(b), EVAL(b, 42), n
? NETERR(), VALTYPE(SETCANCEL()), VALTYPE(NEXTKEY()), DISPCOUNT()
