* LOCAL shadows an outer variable and leaves it alone
x = "outer"
? wrap()
? x
DO sub
? x
RETURN

FUNCTION wrap
LOCAL x
x = "inner"
RETURN peek()

FUNCTION peek
RETURN x

PROCEDURE sub
LOCAL x
x = "sub"
? x
DO sub2
? x
RETURN

PROCEDURE sub2
? x
x = "sub2"
RETURN
