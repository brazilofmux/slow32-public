*> Literals that are not numeric compare for equality only (2023 7.3.8.2
*> rule 1a2).
>>IF "a" > "b"
>>END-IF
identification division.
program-id. ifg.
procedure division.
    stop run.
