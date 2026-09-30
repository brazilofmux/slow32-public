*> Integer arithmetic in a word, checked: a word overflow falls back
*> to the decimal stack for the whole statement (default dialect: COMP-5).
identification division.
program-id. hotarith2.
data division.
working-storage section.
01 a pic s9(8) comp-5.
01 b pic s9(8) comp-5.
01 c pic s9(9) comp.
01 u pic 99 comp-5.
01 d pic s9(18).
01 q pic s9(8) comp-5.
01 k pic s9(4) comp.
procedure division.
    move 100 to a move 7 to b
    compute q = (12 * (a + b) + 373) / 367 display q
    compute u = a - b + 1 display u
    compute u = b - a - 1 display u
    move 2000000000 to a move 2000000000 to b
    compute d = a + b display d
    compute c = a + b display c
    compute d = a * b display d
    compute q = (a * 3) / 7 display q
    compute d = a - (-b) display d
    move -2147483647 to a subtract 1 from a
    compute d = - a display d
    move -1 to b
    divide b into a giving d display d
    move 5 to a move 3 to b
    perform varying k from 1 by 1 until k > 3
        compute c = a * b * k - k display c
    end-perform
    stop run.
