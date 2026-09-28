identification division.
program-id. excloc85.
*> EXCEPTION-LOCATION is COBOL 2002; under -std=85 it is refused.
procedure division.
    display function exception-location
    stop run.
