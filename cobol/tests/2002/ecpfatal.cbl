identification division.
program-id. ecpfatal.
*> A fatal condition in imperative-statement-1 goes only to a WHEN that
*> names it or its hierarchy, never to WHEN OTHER (2023 14.6.13.1.3 rule
*> 4; cobol ISSUES-94 E12): here no WHEN names EC-BOUND-SUBSCRIPT, so the
*> USE declarative runs (rule 5) and the run ends -- "after" is never
*> displayed.
*> No oracle: GnuCOBOL 4 has no exception-checking PERFORM.
data division.
working-storage section.
01  t.
    05 e     pic x occurs 3.
01  k        pic 9 value 4.
procedure division.
declaratives.
ub section.
    use after exception condition ec-bound-subscript.
u1.
    display "  USE EC-BOUND-SUBSCRIPT".
end declaratives.
main section.
m1.
>>TURN EC-BOUND-SUBSCRIPT CHECKING ON
    perform
        move "x" to e(k)
    when exception ec-user-a
        display "  WHEN EC-USER-A (not expected)"
    when other exception
        display "  WHEN OTHER (not expected) " function exception-status
    end-perform
    display "after (not expected)"
    stop run.
