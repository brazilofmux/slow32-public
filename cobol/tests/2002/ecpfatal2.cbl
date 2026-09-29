identification division.
program-id. ecpfatal2.
*> A WHEN phrase that takes a fatal condition and leaves by EXIT PERFORM
*> does not escape the termination (2023 14.6.13.1.3 rule 4: the run ends
*> after the phrase; cobol ISSUES-94 E18): the raise is dropped at the
*> PERFORM's end and, being fatal, ends the run there -- "after" is never
*> displayed.
*> No oracle: GnuCOBOL 4 has no exception-checking PERFORM.
data division.
working-storage section.
01  t.
    05 e     pic x occurs 3.
01  k        pic 9 value 4.
procedure division.
m1.
    perform
        move "x" to e(k)
    when exception ec-bound-subscript
        display "  WHEN EC-BOUND-SUBSCRIPT"
        exit perform
    end-perform
    display "after (not expected)"
    stop run.
