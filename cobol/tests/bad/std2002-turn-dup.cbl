identification division.
program-id. tdup.
*> No exception-name and file-name combination twice in one TURN
*> directive (2023 7.3.25.3 rule 3).
procedure division.
>>TURN EC-SIZE EC-SIZE CHECKING ON
    stop run.
