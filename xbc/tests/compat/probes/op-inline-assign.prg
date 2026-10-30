* Operators: := inline assign, inside an expression too
LOCAL n, m
n := 4
m := (n := n + 1) * 2
? n, m
