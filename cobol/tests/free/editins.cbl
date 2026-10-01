identification division.
program-id. editins.
*> Simple insertion characters (, B 0 /) in edited pictures,
*> X3.23-1985 VI-34, VI-35, editing rules 7 and 8: one embedded in a
*> zero-suppression or floating string, or immediately right of it, is
*> part of the string and takes the replacement character while
*> suppression lasts; one outside such a string is always itself.
*> Found by the differential generator (tests/gen): '0' was never
*> replaced.  The oracle reads the rules differently (docs/oracles.md).
data division.
working-storage section.
01 e1 pic $0$$.99.
01 e2 pic +Z0ZZ9999/99.9.
01 e3 pic Z09,909999.999.
01 e4 pic *0*9999999.999.
01 e5 pic Z/ZZZZZZ.ZZ.
01 e6 pic ***/999999.9999.
01 e7 pic *999.9B99.
01 e8 pic **B909B9999.9999.
01 e9 pic /999.
01 ea pic 0999.
01 eb pic B999,999.
01 ec pic $9+.
01 ed pic 99CR.
01 sn pic s9(8)v9(3).
procedure division.
*> rule 7: a 0 inside a floating string is a space before the symbol
    move 0.42 to e1           display "1 [" e1 "]"
*> rule 8: inside or right of a Z string, while suppression lasts
    move 0 to e2              display "2 [" e2 "]"
    move 6865.48 to e3        display "3 [" e3 "]"
    move 3387.1 to e5         display "5 [" e5 "]"
*> rule 8 with *: the replacement is an asterisk
    move 444 to e4            display "4 [" e4 "]"
    move 27843.419 to e6      display "6 [" e6 "]"
*> a B outside the * string is a space
    move 1472.3191 to e7      display "7 [" e7 "]"
    move 85805 to e8          display "8 [" e8 "]"
*> outside any suppression string: always the character
    move 12 to e9             display "9 [" e9 "]"
    move 12 to ea             display "a [" ea "]"
    move 1234 to eb           display "b [" eb "]"
*> the value edited is the value after truncation, and zero takes the
*> "positive or zero" sign (rule 7; the sign table, VI-34)
    move -32745520.019 to sn
    move sn to ec             display "c [" ec "]"
    move -500 to sn
    move sn to ed             display "d [" ed "]"
    stop run.
