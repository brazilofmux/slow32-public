* Operators: a block outlives the function whose LOCAL it uses (a detached local)
LOCAL add5 := MakeAdder(5), add9 := MakeAdder(9)
? EVAL(add5, 1), EVAL(add9, 1), EVAL(add5, 10)

FUNCTION MakeAdder(n)
RETURN {|x| x + n}
