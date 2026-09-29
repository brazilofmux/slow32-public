identification division.
program-id. ecpturn.
*> A TURN directive inside an exception-checking PERFORM (2023 7.3.25.3
*> rule 5; cobol ISSUES-94 E13). Refused, so the checking after
*> END-PERFORM is exactly the checking before it (14.9.28 rule 22).
procedure division.
m1.
    perform
        display "body"
>>TURN EC-USER-A CHECKING ON
        display "body 2"
    when exception ec-user-a
        display "when"
    end-perform
    stop run.
