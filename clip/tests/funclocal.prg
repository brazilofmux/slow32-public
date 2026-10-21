* LOCAL is not visible to callees; PRIVATE is.
x = "outer"
? wrap()
? x
FUNCTION wrap
LOCAL x
x = "inner"
RETURN peek()
FUNCTION peek
RETURN x
