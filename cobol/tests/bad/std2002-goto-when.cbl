identification division.
program-id. gotowhen.
*> No GO TO in a WHEN phrase of an exception-checking PERFORM (2023
*> 14.9.17.3 rule 3).
data division.
working-storage section.
01 a pic 9 value 1.
procedure division.
p1.
    perform
        compute a = a / 0
    when exception ec-size
        go to p2
    end-perform.
p2.
    stop run.
