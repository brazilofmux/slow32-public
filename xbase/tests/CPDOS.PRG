* A program file that is not UTF-8 is CP437, and is taken as it is
s = "caf‚ Ä"
? "cp437", LEN(s), ASC(SUBSTR(s, 4, 1)), ASC(SUBSTR(s, 6, 1))
