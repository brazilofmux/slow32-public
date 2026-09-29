identification division.
program-id. ecperform.
*> The exception-checking PERFORM (2023 14.9.28 format 3; cobol
*> ISSUES-89).  A WHEN name is turned on for imperative-statement-1 alone
*> (general rule 14); a condition raised there goes to its WHEN phrase,
*> not to a USE declarative (17), and a nonfatal one resumes after the
*> statement it arose in (20).  WHEN OTHER takes any other enabled
*> condition and ends the PERFORM (18); WHEN COMMON follows either (19);
*> FINALLY is the end of the PERFORM (16).  After END-PERFORM the names
*> turned on for it are off again (22).  A fatal condition runs its WHEN
*> phrase and then ends the run.
*> No oracle (docs/standards.md).
data division.
working-storage section.
01  t.
    05 e     pic x occurs 3.
01  k        pic 9 value 4.
procedure division.
declaratives.
user-a section.
    use after exception condition ec-user-a.
u1.
    display "  USE for EC-USER-A".
end declaratives.
main section.
m1.
>>TURN EC-USER-B CHECKING ON
    perform with location
        display "1: before"
        raise exception ec-user-a
        display "1: after the raise, resumed"
        raise exception ec-user-b
        display "1: not reached (WHEN OTHER ends the PERFORM)"
    when exception ec-user-a
        display "  WHEN EC-USER-A [" function exception-location "]"
    when other exception
        display "  WHEN OTHER: " function exception-status(1:9)
    when common exception
        display "  WHEN COMMON"
    finally
        display "1: finally"
    end-perform
    display "2: after END-PERFORM, EC-USER-A is off again:"
    raise exception ec-user-a
>>TURN EC-USER-A CHECKING ON
    display "3: turned on outside, its USE declarative runs:"
    raise exception ec-user-a
    display "4: a fatal condition in a PERFORM:"
    perform
        move "x" to e(k)
        display "4: not reached"
    when exception ec-bound-subscript
        display "  WHEN EC-BOUND-SUBSCRIPT, then the run ends"
    end-perform
    display "not reached"
    stop run.
