identification division.
program-id. natedit.
*> National-edited pictures (2023 13.18.40; cobol ISSUES-73): N with the
*> insertion symbols B, 0 and /.  A MOVE fills the N positions left to
*> right and inserts a national space, zero or stroke at the others; a
*> figurative constant and ALL are edited too.  As a sender the item is
*> its characters, insertions included.  LENGTH counts every position.
*> No oracle (docs/national.md).
data division.
working-storage section.
01  d        pic nn/nn/nnnn.
01  s        pic n(3)bn(3).
01  z        pic nn0nn.
01  v        pic nnbnn value n"ab cd".
01  n        pic n(10).
01  k        pic 99.
procedure division.
main.
    move n"09282026" to d
    display "date: [" d "]"
    move "東京大阪" to s
    display "space inserted: [" s "]"
    move n"1234" to z
    display "zero inserted: [" z "]"
    move 42 to z
    display "from an integer: [" z "]"
    move spaces to d
    display "spaces: [" d "]"
    move all n"x" to d
    display "all: [" d "]"
    move d to n
    display "as a sender: [" n "]"
    display "value: [" v "] " function length(v)
    move 0 to k
    inspect d tallying k for all n"/"
    display "strokes: " k
    stop run.
