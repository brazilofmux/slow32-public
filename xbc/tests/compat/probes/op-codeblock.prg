* Operators: { } code blocks, EVAL with arguments
LOCAL b := {|x, y| x * 10 + y}
? EVAL(b, 3, 4)
? EVAL({|| "no args"})
