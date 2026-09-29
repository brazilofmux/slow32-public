identification division.
program-id. ecpdup.
*> An exception-name in two WHEN phrases of one exception-checking
*> PERFORM (2023 14.9.28.3 rule 15; cobol ISSUES-94 E16).
procedure division.
m1.
    perform
        raise exception ec-user-a
    when exception ec-user-a
        display "first"
    when exception ec-user-a
        display "second"
    end-perform
    stop run.
