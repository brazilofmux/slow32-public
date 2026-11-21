* A number shown as zero has no minus sign in a compiled program too: a
* sum that should net to zero lands a hair below it (0.3 - 0.1 - 0.2 is
* about -2.8E-17).  dBASE III PLUS shows B, C and the first of D as 0.00;
* FoxPro 2.6 shows B and C so (xbase ISSUES).
SET TALK OFF
n = 0.3 - 0.1 - 0.2
? "A", STR(n, 12, 2)
? "B", STR(-0.001, 10, 2)
? "C", TRANSFORM(n, "999,999.99")
? "D", TRANSFORM(-0.004, "99.99"), TRANSFORM(-0.006, "99.99")
? "E", STR(-1.5, 8, 2), STR(-0.0004, 8, 3)
