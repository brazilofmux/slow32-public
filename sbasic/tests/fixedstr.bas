' fixed-length strings: n CHR$(0)s at first, every store cut or
' padded with spaces -- checked against QBasic 1.1
TYPE t
n AS STRING * 4
k AS INTEGER
END TYPE
DIM r AS t
PRINT LEN(r.n); ASC(r.n)
DIM s AS STRING * 3
PRINT LEN(s); ASC(s)
s = "abcdef": PRINT "["; s; "]"
s = "x": PRINT "["; s; "]"
r.n = "hi": PRINT "["; r.n; "]"; LEN(r.n)
DIM a(2) AS STRING * 2
PRINT LEN(a(1)); ASC(a(1))
a(1) = "pqr": PRINT "["; a(1); "]"
DIM q(2) AS t
PRINT ASC(q(2).n)
s = s + "z": PRINT "["; s; "]"
MID$(s, 2) = "QRST": PRINT "["; s; "]"
LSET s = "m": PRINT "["; s; "]"
RSET s = "m": PRINT "["; s; "]"
CALL f(s): PRINT "["; s; "]"
DIM u AS STRING
u = "abcde": SWAP s, u: PRINT "["; s; "]["; u; "]"
SUB f (x$)
x$ = "longer": PRINT LEN(x$)
END SUB
