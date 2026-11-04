' DEFINT and its kind in s32-bc: each before the names of its letters
DEFINT I-N
DEFDBL D
DEFSTR S
DEFLNG L
I = 7.6: J = I / 2: K = I \ 2: PRINT I; J; K
D = 1 / 3: PRINT D
S = "str": PRINT S + "ing"
L = 100000: PRINT L * 3
FOR M = 1 TO 3: PRINT M;: NEXT: PRINT M
DIM N(5), DA(2), SA(2)
N(1) = 2.5: DA(1) = 1 / 3: SA(1) = "x": PRINT N(1); DA(1); SA(1)
IM(2) = 9.7: PRINT IM(2)
READ SR, I2, SR2: PRINT SR; I2; SR2
DEF FNI(A) = A * 2: PRINT FNI(1.6)
DEF FNS(X$) = X$ + "!": PRINT FNS("hi")
X = 2.5: PRINT X
DATA word, 3.5, other
