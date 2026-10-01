identification division.
program-id. abbrnot.
*> Abbreviated combined relation conditions: the five examples X3.23-1985
*> VI-61 gives with their expanded equivalents, each evaluated both ways
*> over every a, b, c, d in 1..3; a line is printed only if the two
*> disagree.  The last stated relational operator, NOT included, is the
*> one an object standing alone takes; it was not recorded after an
*> abbreviation that stated its own (found by the differential
*> generator, tests/gen).
data division.
working-storage section.
01 a pic 9.
01 b pic 9.
01 c pic 9.
01 d pic 9.
01 x pic 9.
01 y pic 9.
01 bad pic 9(4) value 0.
procedure division.
    perform varying a from 1 by 1 until a > 3
     perform varying b from 1 by 1 until b > 3
      perform varying c from 1 by 1 until c > 3
       perform varying d from 1 by 1 until d > 3
        move 0 to x y
        if a > b and not < c or d move 1 to x end-if
        if ((a > b) and (a not < c)) or (a not < d) move 1 to y end-if
        if x not = y display "1 " a b c d " " x y add 1 to bad end-if
        move 0 to x y
        if a not equal b or c move 1 to x end-if
        if (a not equal b) or (a not equal c) move 1 to y end-if
        if x not = y display "2 " a b c d " " x y add 1 to bad end-if
        move 0 to x y
        if not a = b or c move 1 to x end-if
        if (not (a = b)) or (a = c) move 1 to y end-if
        if x not = y display "3 " a b c d " " x y add 1 to bad end-if
        move 0 to x y
        if not (a greater b or < c) move 1 to x end-if
        if not ((a greater b) or (a < c)) move 1 to y end-if
        if x not = y display "4 " a b c d " " x y add 1 to bad end-if
        move 0 to x y
        if not (a not > b and c and not d) move 1 to x end-if
        if not ((((a not > b) and (a not > c))) and (not (a not > d))) move 1 to y end-if
        if x not = y display "5 " a b c d " " x y add 1 to bad end-if
       end-perform
      end-perform
     end-perform
    end-perform
    display "disagreements: " bad
    stop run.
