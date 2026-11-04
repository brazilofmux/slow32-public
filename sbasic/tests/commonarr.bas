' COMMON SHARED with arrays, as QBasic takes it: a(), b() AS INTEGER,
' c$(), r() AS a record, f() AS STRING * 3 -- each the main program's
' in every SUB and FUNCTION once its DIM (after) makes it; the COMMON's
' type is its DIM's and its REDIM's without AS; REDIM in a SUB, ERASE
' and DIM again seen by them all -- the values printed
DECLARE SUB fill ()
DECLARE SUB grow ()
DECLARE FUNCTION total& ()
TYPE cell
  tag AS STRING * 2
  n AS INTEGER
END TYPE
COMMON SHARED a(), b() AS INTEGER, c$(), k, r() AS cell, f() AS STRING * 3
DIM a(5), c$(2), r(2) AS cell, f(1) AS STRING * 3
REDIM b(3)
k = 7
fill
PRINT a(1); a(5); b(2); c$(1); "|"; c$(2); k; r(1).tag; r(1).n; f(1); "|"
b(1) = 2.6
PRINT b(1)
grow
PRINT UBOUND(b); b(6); total&
ERASE a
DIM a(2 TO 4)
a(3) = 9
fill
PRINT LBOUND(a); a(3); a(4); r(1).n
END
SUB fill
FOR i = LBOUND(a) TO UBOUND(a): a(i) = a(i) + i * 10: NEXT
b(2) = b(2) + 1
c$(1) = "q" + c$(1): c$(2) = c$(2) + "v"
k = k + 1
r(1).tag = "zz": r(1).n = r(1).n + 4
f(1) = f(1) + "pqrs"
END SUB
SUB grow
REDIM b(8)
b(6) = 66.4
END SUB
FUNCTION total&
FOR i = 0 TO UBOUND(b): t& = t& + b(i): NEXT
total& = t& + UBOUND(a)
END FUNCTION
