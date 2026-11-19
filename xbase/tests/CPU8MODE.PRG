* SET CODEPAGE TO "UTF8" (docs/plans/xbase-utf8.md): the string functions count
* code points, case maps Unicode's, CHR and ASC are code points; the file is
* UTF-8, and its own text is what a switch converts from, so nothing
* CP437 lacks is lost to the load
SET TALK OFF
SET CODEPAGE TO "UTF8"
s = "Ça va? Žluťoučký kůň 日本"
? "len", LEN(s), LEN("é"), LEN("")
? "substr", SUBSTR(s, 1, 2), SUBSTR(s, 9, 9), SUBSTR(s, -2)
? "left/right", LEFT(s, 3), RIGHT(s, 2)
? "at/rat", AT("ž", LOWER(s)), AT("日", s), RAT("ů", s)
? "upper", UPPER(s)
? "lower", LOWER(s)
? "proper", PROPER("élan vital ÉCOLE")
? "pad", "[" + PADL("né", 5) + "]", "[" + PADR("né", 5, "·") + "]", "[" + PADC("日", 5) + "]"
? "stuff", STUFF("naïve", 3, 1, "ï-ï")
? "chr/asc", CHR(233), ASC("é"), ASC("日"), CHR(26085)
? "is", ISALPHA("é"), ISUPPER("É"), ISLOWER("é"), ISALPHA("1")
? "chrtran", CHRTRAN("café crème", "éè", "ee")
SET CODEPAGE TO "CP437"
? "back", LEN("é"), ASC("é")
