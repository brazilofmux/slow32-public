' WHILE and DO loops over an array: insertion sort, a scan
DEFINT A-Z
CONST N = 300
DIM a(N)
s& = 7
i = 1
WHILE i <= N
  s& = (s& * 31 + 11) MOD 10007
  a(i) = s& MOD 1000
  i = i + 1
WEND
i = 2
DO WHILE i <= N
  v = a(i): j = i - 1
  DO WHILE j >= 1
    IF a(j) <= v THEN EXIT DO
    a(j + 1) = a(j)
    j = j - 1
  LOOP
  a(j + 1) = v
  i = i + 1
LOOP
PRINT a(1); a(N \ 2); a(N)
