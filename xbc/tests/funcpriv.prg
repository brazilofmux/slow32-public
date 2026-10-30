* PRIVATE restores the caller's memvar
x = "outer"
? wrap()
? x
FUNCTION wrap
PRIVATE x
x = "inner"
RETURN x
