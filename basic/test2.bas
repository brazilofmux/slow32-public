rem === MY-BASIC Test Suite for SLOW-32 ===
rem Tests basic arithmetic, strings, control flow

print "=== SLOW-32 MY-BASIC Test ==="

rem Arithmetic
print "1+2="; 1+2
print "10-3="; 10-3
print "4*5="; 4*5
print "15/3="; 15/3

rem Variables
a = 42
b = 8
print "a="; a
print "b="; b
print "a+b="; a+b
print "a*b="; a*b

rem String operations
s$ = "Hello"
print s$; " SLOW-32!"

rem Control flow - IF/THEN/ELSE
x = 10
if x > 5 then
  print "x is greater than 5"
else
  print "x is not greater than 5"
endif

rem FOR loop
print "Counting: ";
for i = 1 to 5
  print i; " ";
next
print ""

rem WHILE loop
n = 1
while n <= 4
  print "n="; n
  n = n + 1
wend

print "=== All tests done ==="
