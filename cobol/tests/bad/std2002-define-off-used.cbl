*> After >>DEFINE ... OFF a variable is used only in a defined condition
*> (2023 7.3.11.4 rule 2).
>>DEFINE V AS 1
>>DEFINE V OFF
>>IF V = 1
>>END-IF
identification division.
program-id. defo.
procedure division.
    stop run.
