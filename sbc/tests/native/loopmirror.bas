' loops whose arrays and counters the compiler may hold in registers, and
' the ones it may not: Overflow at a counter's limit, a counter the body
' changes, loops left and entered by GOTO, loops that do not run, subscripts
' out of range in them, arrays moved (ERASE, REDIM) elsewhere, a DIM after
' a jump, a FUNCTION in the body changing an array -- the trail printed
DECLARE FUNCTION touch% (k%)
DECLARE SUB grow ()
DEFINT A-Z
CONST N = 5
DIM SHARED sh(N)
m = 4
DIM st(10), cn(N, 2), dy(m)
REDIM mv(3)
ON KEY(1) GOSUB nokey
t& = 0
FOR i = 0 TO 10: st(i) = i * i: NEXT
FOR i = 0 TO N: FOR j = 0 TO 2: cn(i, j) = i + j: t& = t& + cn(i, j) * st(i): NEXT: NEXT
PRINT t&
FOR i = 5 TO 1: st(99) = 1: NEXT
PRINT "zero-trip"; i
FOR i = 10 TO 0 STEP -2: t& = t& + st(i): NEXT
PRINT t&; i
FOR i = 1 TO 10
  IF i = 4 THEN i = 8
  t& = t& + i
NEXT
PRINT t&; i
FOR i = 1 TO 3
  FOR i2 = 1 TO 2: t& = t& + st(i2): NEXT
  IF i = 2 THEN GOTO outside
NEXT
outside:
PRINT t&; i; i2
k = 0
back:
FOR i = 1 TO 2: st(i) = st(i) + 1: NEXT
k = k + 1: IF k < 3 THEN GOTO back
PRINT st(1); st(2)
FOR i = 1 TO 3
  x = touch(i)
  t& = t& + sh(i)
NEXT
PRINT t&
FOR i = 1 TO 3
  mv(i) = i
NEXT
PRINT mv(3); UBOUND(mv)
grow
FOR i = 1 TO 6: mv(i) = i * 2: NEXT
PRINT mv(6); UBOUND(mv)
ERASE dy
REDIM dy(2)
FOR i = 0 TO 2: dy(i) = 7 + i: NEXT
PRINT dy(2)
FOR q& = 2147483640 TO 2147483644 STEP 3: t& = q& - 2147483000: NEXT
PRINT t&; q&
FOR i = 32760 TO 32766: st(0) = i: NEXT
PRINT st(0); i
FOR b = -32768 TO -32760 STEP 4: st(1) = b: NEXT
PRINT st(1); b
ON ERROR GOTO 0
PRINT "next: Overflow at the limit"
FOR i = 32766 TO 32767: PRINT i;: NEXT
PRINT "not reached"
END
nokey: RETURN
FUNCTION touch% (k%)
sh(k%) = k% * 100
touch% = 0
END FUNCTION
SUB grow
SHARED mv()
REDIM mv(6)
END SUB
