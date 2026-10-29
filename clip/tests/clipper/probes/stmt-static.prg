* Statements: STATIC keeps its value between calls
? Counter(), Counter(), Counter()

FUNCTION Counter()
STATIC n := 0
n++
RETURN n
