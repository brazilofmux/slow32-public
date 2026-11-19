* A program file in UTF-8 (docs/plans/xbase-utf8.md, ruling 6): its text is
* converted to the program's codepage, CP437, on load -- a character
* CP437 lacks becomes ?
s = "café ─ 日"
? "utf8", LEN(s), ASC(SUBSTR(s, 4, 1)), ASC(SUBSTR(s, 6, 1)), SUBSTR(s, 8, 1)
