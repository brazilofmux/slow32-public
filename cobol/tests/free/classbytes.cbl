*> The class conditions of alphanumeric bytes (NUMERIC, ALPHABETIC,
*> ALPHABETIC-LOWER, ALPHABETIC-UPPER), which the compiler tests itself
*> when the operand is one character and hands to the runtime's loop
*> with an address and a length otherwise (docs/performance.md).  Every
*> byte value as a one-character item, as a part of one character, and
*> as each position of a longer part of literal and of computed length:
*> how many of the 256 are in each class, and which are the first and
*> the last -- 10 digits, 0 to 9; 52 letters and the space, 27 of each
*> case with it.  And the conditions negated, which must count the rest.
identification division.
program-id. classbytes.
data division.
working-storage section.
01  b            pic x.
01  s            pic x(8).
01  i            pic 9(4) comp.
01  p            pic 9(4) comp.
01  n            pic 9(4) comp.
01  k            pic 9(4) comp.
01  cnt.
    05  c        pic 9(4) comp occurs 4.
01  nots.
    05  nc       pic 9(4) comp occurs 4.
01  firsts.
    05  f        pic 9(4) comp occurs 4.
01  lasts.
    05  l        pic 9(4) comp occurs 4.
01  hit          pic 9.
procedure division.
main-para.
    *> one character, an item
    perform clear
    perform varying i from 1 by 1 until i > 256
        move function char(i) to b
        if b is numeric move 1 to k perform tally-k end-if
        if b is alphabetic move 2 to k perform tally-k end-if
        if b is alphabetic-lower move 3 to k perform tally-k end-if
        if b is alphabetic-upper move 4 to k perform tally-k end-if
        if b is not numeric add 1 to nc(1) end-if
        if b is not alphabetic add 1 to nc(2) end-if
        if b is not alphabetic-lower add 1 to nc(3) end-if
        if b is not alphabetic-upper add 1 to nc(4) end-if
    end-perform
    display "item:"
    perform show
    *> one character, a part: each position of s in turn
    perform clear
    perform varying i from 1 by 1 until i > 256
        compute p = function mod(i, 8) + 1
        move all "~" to s
        move function char(i) to s(p:1)
        if s(p:1) is numeric move 1 to k perform tally-k end-if
        if s(p:1) is alphabetic move 2 to k perform tally-k end-if
        if s(p:1) is alphabetic-lower move 3 to k perform tally-k end-if
        if s(p:1) is alphabetic-upper move 4 to k perform tally-k end-if
        if s(p:1) not numeric add 1 to nc(1) end-if
        if s(p:1) not alphabetic add 1 to nc(2) end-if
        if s(p:1) not alphabetic-lower add 1 to nc(3) end-if
        if s(p:1) not alphabetic-upper add 1 to nc(4) end-if
    end-perform
    display "part of one:"
    perform show
    *> a part of four, of literal length: one byte of it varies, the
    *> rest in the class
    perform clear
    perform varying i from 1 by 1 until i > 256
        compute p = function mod(i, 4) + 3
        move "~~4444~~" to s  move function char(i) to s(p:1)
        if s(3:4) is numeric move 1 to k perform tally-k end-if
        if s(3:4) is not numeric add 1 to nc(1) end-if
        move "~~mM M~~" to s  move function char(i) to s(p:1)
        if s(3:4) is alphabetic move 2 to k perform tally-k end-if
        if s(3:4) is not alphabetic add 1 to nc(2) end-if
        move "~~mm m~~" to s  move function char(i) to s(p:1)
        if s(3:4) is alphabetic-lower move 3 to k perform tally-k end-if
        if s(3:4) is not alphabetic-lower add 1 to nc(3) end-if
        move "~~MM M~~" to s  move function char(i) to s(p:1)
        if s(3:4) is alphabetic-upper move 4 to k perform tally-k end-if
        if s(3:4) is not alphabetic-upper add 1 to nc(4) end-if
    end-perform
    display "part of four:"
    perform show
    *> the same with the length computed, and the start
    perform clear
    perform varying i from 1 by 1 until i > 256
        compute n = function mod(i, 5) + 2
        compute p = function mod(i, n) + 2
        move "~444444~" to s  move function char(i) to s(p:1)
        if s(2:n) is numeric move 1 to k perform tally-k end-if
        if s(2:n) is not numeric add 1 to nc(1) end-if
        move "~mM Mm ~" to s  move function char(i) to s(p:1)
        if s(n - n + 2:n) is alphabetic move 2 to k perform tally-k end-if
        if s(n - n + 2:n) is not alphabetic add 1 to nc(2) end-if
        move "~mm mm ~" to s  move function char(i) to s(p:1)
        if s(2:n) is alphabetic-lower move 3 to k perform tally-k end-if
        if s(2:n) is not alphabetic-lower add 1 to nc(3) end-if
        move "~MM MM ~" to s  move function char(i) to s(p:1)
        if s(2:n) is alphabetic-upper move 4 to k perform tally-k end-if
        if s(2:n) is not alphabetic-upper add 1 to nc(4) end-if
    end-perform
    display "part of a computed length:"
    perform show
    *> a whole item of eight, and a part to the end of the item
    move "12345678" to s
    if s is numeric display "eight digits: numeric" end-if
    move "1234567 " to s
    if s is not numeric display "seven and a space: not" end-if
    if s(1:7) is numeric display "its first seven: numeric" end-if
    if s(8:) is alphabetic display "its last, to the end: alphabetic" end-if
    if s(7:) is not alphabetic display "its last two: not" end-if
    stop run.

clear.
    move 0 to k
    perform varying k from 1 by 1 until k > 4
        move 0 to c(k) nc(k) f(k) l(k)
    end-perform.

tally-k.
    add 1 to c(k)
    if f(k) = 0 compute f(k) = i - 1 end-if
    compute l(k) = i - 1.

show.
    display "  numeric " c(1) " not " nc(1) " from " f(1) " to " l(1)
    display "  alphabetic " c(2) " not " nc(2) " from " f(2) " to " l(2)
    display "  lower " c(3) " not " nc(3) " from " f(3) " to " l(3)
    display "  upper " c(4) " not " nc(4) " from " f(4) " to " l(4).
