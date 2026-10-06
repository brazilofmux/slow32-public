*> THROUGH in an >>EVALUATE takes numeric operands (2023 7.3.13.3 rule 12).
>>EVALUATE "b"
>>WHEN "a" THROUGH "c"
>>END-EVALUATE
identification division.
program-id. evt.
procedure division.
    stop run.
