*> RESUME (2023 14.9.33; docs/plans/standard-queue.md item 42): AT NEXT
*> STATEMENT from a declarative taking a fatal condition (EC-BOUND-REF-MOD)
*> twice -- the run goes on after the statement; AT a nondeclarative
*> procedure; AT NEXT STATEMENT inside a WHEN phrase of an exception-
*> checking PERFORM. No oracle: GnuCOBOL 4 has no RESUME.
identification division.
program-id. resume.
data division.
working-storage section.
01 t pic x(5) value "abcde".
01 n pic 9 value 9.
01 k pic 9(3) value 7.
01 cnt pic 9 value 0.
procedure division.
declaratives.
bound-sec section.
    use after exception condition ec-bound-ref-mod.
bound-para.
    display "declarative: " function exception-status.
    add 1 to cnt.
    if cnt = 1 resume at next statement.
    resume at recovery.
end declaratives.
main section.
    >>turn ec-bound-ref-mod ec-size-zero-divide checking on
    move t(n:1) to t.
    display "after the first raise, cnt=" cnt.
    move t(n:1) to t.
    display "not reached".
recovery.
    display "recovery: cnt=" cnt.
    perform
        divide 5 by 0 giving k
        display "after the divide: k=" k
        compute k = 5 / 0
        display "after the compute"
    when exception ec-size-zero-divide
        display "when: " function exception-status
        resume at next statement
    end-perform.
    display "done".
    stop run.
