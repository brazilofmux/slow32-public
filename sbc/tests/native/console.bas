' PRINT USING, WRITE, LINE INPUT, MID$ =, CLS, LOCATE, COLOR, WIDTH,
' SLEEP, BEEP, CLEAR in s32-bc
PRINT USING "##.##"; 3.14159
PRINT USING "Total: $$#,###.## and !"; 1234.5; "yes"
PRINT USING "\   \|"; "abcdefgh"; "xy"; "z"
PRINT USING "#### _# &"; 12; 34; "lit"
F$ = "+##.#": PRINT USING F$; 1.25; -2.75; 99.99
PRINT USING "no fields"; 5
PRINT USING "##"; 123456
WRITE 1, 2.5, "three", -4
WRITE
LINE INPUT "Name? "; N$: PRINT "["; N$; "]"
LINE INPUT A$: PRINT LEN(A$)
S$ = "abcdef": MID$(S$, 2) = "XY": PRINT S$
MID$(S$, 4, 1) = "QRS": PRINT S$
MID$(S$, 9) = "never": PRINT S$
MID$(S$, 1, 0) = "Z": PRINT S$
COLOR 2, 0: CLS: LOCATE 3, 5: PRINT "at 3,5": COLOR 7
WIDTH 40: SLEEP 0: BEEP
X = 5: Y$ = "keep": DIM Q(3): Q(1) = 9
CLEAR
PRINT X; "["; Y$; "]"; Q(1)
