* Functions: TRANSFORM() on character, date and logical values -- the
* function string (@! @R @S<n> @D @E, and the GET-only @L / @Y, ignored)
* and the template (A N X 9 # ! take a character; other characters
* overwrite, or with @R are inserted; L / Y format a logical)
LOCAL c := "abc123xyz", d := CTOD("03/15/1992"), i
LOCAL cp := {"", "@!", "@R XX-XX", "XX-XX", "@R 999-999", "@! XXX", "!!!", "@S4", "@R! X.X.X", "@B", "AAA", "999", "@X", "XXXXXXXXXXXXX", "@R XXXXXXXXXXXXX"}
LOCAL dp := {"", "@D", "@E", "99/99/99", "XXXXXXXX", "@B"}
LOCAL lp := {"", "@L", "@Y", "@LXX", "L", "Y", "X", "@R L!", "LLL", "YY"}
FOR i := 1 TO LEN(cp)
   ? "C", PADR(cp[i], 16), "[" + TRANSFORM(c, cp[i]) + "]"
NEXT
FOR i := 1 TO LEN(dp)
   ? "D", PADR(dp[i], 16), "[" + TRANSFORM(d, dp[i]) + "]"
NEXT
FOR i := 1 TO LEN(lp)
   ? "L", PADR(lp[i], 16), "[" + TRANSFORM(.T., lp[i]) + "]", "[" + TRANSFORM(.F., lp[i]) + "]"
NEXT
