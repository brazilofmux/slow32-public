identification division.
program-id. raise85.
*> RAISE is COBOL 2002 (14.9.29): refused under -std=85.
procedure division.
    raise exception ec-user-x
    stop run.
