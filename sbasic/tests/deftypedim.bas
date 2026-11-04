REM DIM after DEFINT: QuickBASIC makes an INTEGER array (checked
REM against QBasic 1.1); a suffix or AS keeps its own type
DEFINT A-Z
DIM A(10)
A(1) = 2.7
B = 3.6
C(2) = 4.4
PRINT A(1); B; C(2)
N = 4
DIM D(N), F(2) AS DOUBLE, G#(2)
D(1) = 5.5: F(1) = 5.5: G#(1) = 6.5
PRINT D(1); F(1); G#(1)
