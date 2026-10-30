* Operators: a block passed down still sees its maker's LOCAL, not the callee's
LOCAL n := 7, b := {|x| x + n}
? Apply(b, 1)

FUNCTION Apply(blk, v)
LOCAL n := 1000
RETURN EVAL(blk, v)
