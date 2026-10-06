identification division.
program-id. dirx.
*> A compiler directive this compiler does not implement yet is refused
*> by name (>>DEFINE, >>IF and >>EVALUATE are implemented since
*> 2026-10-06; >>COBOL-WORDS is queued with the 2023 directives).
>>COBOL-WORDS RESERVE "XYZ"
procedure division.
    stop run.
