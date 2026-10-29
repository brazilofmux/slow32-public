* Operators: @ passes a variable by reference to a function
LOCAL n := 1
Bump(@n)
? n
Bump(n)
? n

FUNCTION Bump(x)
x := x + 100
RETURN NIL
