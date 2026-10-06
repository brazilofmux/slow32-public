*> A second >>DEFINE with another value needs OFF first, or OVERRIDE
*> (2023 7.3.11.3 rule 2).
>>DEFINE V AS 1
>>DEFINE V AS 2
identification division.
program-id. defr.
procedure division.
    stop run.
