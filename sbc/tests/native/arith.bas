' s32-bc's native paths, held to the interpreter (run-oracle.py --bc)
A% = 32767: B% = -32768: L& = 2147483647: M& = -2147483647
PRINT A% - 1; B% + 1; A% + B%; A% * 1; B% \ 2; B% MOD 7
PRINT L& - 1; M& - 1; L& \ 3; L& MOD 1000; A% * 2&; A% * A%
PRINT 7 \ 2; -7 \ 2; 7 MOD -3; -7 MOD 3; 17 \ 1; 17 MOD 5
PRINT 1 / 3; 2 / 3#; 10 / 4; A% / 2; L& / 7; 1# / 3
PRINT 5 AND 3; 5 OR 3; 5 XOR 3; 5 IMP 3; 5 EQV 3; NOT 5; NOT A%
PRINT 70000 AND 65535; L& OR 1; -1 AND 32767; NOT L&
PRINT 3 < 4; 3 > 4; 3 = 3; 3 <> 3; 2.5 <= 2.5; 2.5 >= 2.6; 1 < 1.5; 1# = 1
PRINT -A%; -B% + 1; -L&; -2.5; -(1 / 3)
X = 1 / 3: Y# = X: Z# = 1 / 3: PRINT X; Y#; Z#; X * 3; Y# * 3
S = 16777217: PRINT S; S + 1: T# = 16777217: PRINT T#
Q = 0: IF Q THEN PRINT "Q true" ELSE PRINT "Q false"
Q = -0: IF Q THEN PRINT "-0 true" ELSE PRINT "-0 false"
Q = .0001: IF Q THEN PRINT "small true"
I% = 7: IF I% AND 4 THEN PRINT "bit 4"
C% = 2.5: D% = 3.5: E% = -2.5: F& = 123456.5: PRINT C%; D%; E%; F&
G = 1E+20: H# = G * G: PRINT G; H#
P = 2 ^ 10: R# = 2# ^ .5: PRINT P; R#
FOR I = 1 TO 3: PRINT I;: NEXT: PRINT
FOR I = 3 TO 1 STEP -1: PRINT I;: NEXT: PRINT I
FOR I = 1 TO 0: PRINT "never": NEXT: PRINT I
FOR I = 0 TO 1 STEP .1: N = N + 1: NEXT: PRINT N; I
FOR J% = 1 TO 10 STEP 3: PRINT J%;: NEXT: PRINT J%
FOR J% = 1 TO 3 STEP .6: PRINT J%;: NEXT: PRINT J%
K = -2: FOR J% = 5 TO 1 STEP K: PRINT J%;: NEXT: PRINT J%
K = 0: N = 0: FOR J% = 1 TO 1 STEP K: N = N + 1: IF N > 3 THEN J% = 2
NEXT: PRINT N; J%
FOR D# = 1 TO 2 STEP .25: PRINT D#;: NEXT: PRINT
FOR L& = 2147483640 TO 2147483646 STEP 3: PRINT L&;: NEXT: PRINT
LM = 4: FOR I = 1 TO LM: LM = 2: PRINT I;: NEXT: PRINT
DEF FNA%(X%) = X% * 2 + 1
DEF FNB#(X#) = X# / 3
DEF FNC(X, Y) = X * Y + 0.1
DEF FND$(A$) = A$ + "!"
PRINT FNA%(10); FNB#(1); FNC(2, 3); FND$("hi"); FNA%(2.5)
