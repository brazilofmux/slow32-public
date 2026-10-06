*> A compile-time arithmetic expression cannot divide by zero (2023
*> 7.3.6.2 rule 1c).
>>DEFINE V AS 4 / 0
identification division.
program-id. defz.
procedure division.
    stop run.
