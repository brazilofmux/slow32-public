* A GET in UTF-8 (docs/plans/xbase-utf8.md): the field is characters, eight
* of "naive" with its diaeresis; a CJK character typed over the first, End,
* o-umlaut typed after the word.  KEYCHAR(): a key past U+00FF is its code
* point plus 65536, so Alt-A (286) is no character.
SET TALK OFF
SET CODEPAGE TO "UTF8"
x = "naïve   "
CLEAR
@ 1,1 GET x
READ
? "[" + x + "]", LEN(x), KEYCHAR(65536 + 26085), KEYCHAR(233), "[" + KEYCHAR(286) + "]"
