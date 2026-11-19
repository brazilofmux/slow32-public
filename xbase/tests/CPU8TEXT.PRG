* The text functions in UTF-8 (docs/plans/xbase-utf8.md): TRANSFORM's
* template by character; MEMOLINE and MLCOUNT wrap at a width in display
* columns (a CJK character takes two); MLPOS counts characters; HARDCR and
* MEMOTRAN leave alone a 141 that is inside a character (c-caron is C4 8D)
SET TALK OFF
SET CODEPAGE TO "UTF8"
? "transform", TRANSFORM("naïve", "@!"), TRANSFORM("ab日c", "X-X-X-X"), TRANSFORM("éa", "!!")
t = "日本語の文 テキスト"
? "mlcount", MLCOUNT(t, 6), MLCOUNT("aé bü cö", 4)
? "memoline", "[" + MEMOLINE(t, 6, 1) + "]", "[" + MEMOLINE(t, 6, 2) + "]", "[" + MEMOLINE("aé bü", 4, 1) + "]"
? "mlpos", MLPOS("aé bü cö", 4, 2), MLPOS("aé bü cö", 4, 3)
s = "Brač" + CHR(13) + CHR(10) + "ok"
? "hardcr", LEN(HARDCR(s)), HARDCR(s) == s
? "memotran", MEMOTRAN("Brač" + CHR(13) + CHR(10) + "nač" + BCHR(141) + CHR(10) + "x")
