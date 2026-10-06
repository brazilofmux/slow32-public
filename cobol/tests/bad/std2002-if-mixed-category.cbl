*> A constant conditional expression compares operands of one category
*> (2023 7.3.8.2 rule 1a1).
>>IF 1 = "1"
>>END-IF
identification division.
program-id. ifm.
procedure division.
    stop run.
