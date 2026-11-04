' arrays of records, and records copied (QBasic: b = a copies the
' fields; SWAP exchanges them) -- checked against QBasic 1.1
TYPE pt
x AS INTEGER
y AS INTEGER
END TYPE
DIM a AS pt, b AS pt
a.x = 7: b = a: b.x = 9
PRINT a.x; b.x
SWAP a, b
PRINT a.x; b.x
SWAP a.x, b.x
PRINT a.x; b.x
DIM c(3) AS pt
c(1) = a: c(2).x = 5: c(1).y = c(2).x + 1
PRINT c(1).x; c(1).y; c(2).x; c(0).x
SWAP c(1), c(2)
PRINT c(1).x; c(2).x; c(2).y
b = c(2): PRINT b.x; b.y
