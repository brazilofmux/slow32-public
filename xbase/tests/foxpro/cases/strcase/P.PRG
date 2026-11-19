* strings compare byte for byte: case counts, with SET EXACT off and on
x = "abc"
y = "ABC"
? "A", x == y
? "B", x = y
? "C", x < y
? "D", x > y
? "E", "Zebra" < "apple", "a" > "Z"
SET EXACT ON
? "F", x = y
? "G", "abc" = "ab", "ab" = "abc"
SET EXACT OFF
? "H", "abc" = "ab", "ab" = "abc"
SET EXACT ON
? "I", "abc" = "abc  ", "abc  " = "abc", "abc" = " abc", "   " = ""
? "J", "abc" < "abc  ", "abc" >= "abc  ", "abc" <> "abc  "
SET EXACT OFF
? "K", "abc" = "", "" = "abc", "abc  " = "abc", "abc" < "abc  "
