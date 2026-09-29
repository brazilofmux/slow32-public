identification division.
program-id. ex85.
*> EXIT PERFORM is COBOL 2002; under -std=85 it is refused.
procedure division.
    perform 2 times
        exit perform
    end-perform
    stop run.
