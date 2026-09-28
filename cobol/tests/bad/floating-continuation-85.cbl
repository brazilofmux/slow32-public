identification division.
program-id. fc85.
*> The floating literal continuation "- is COBOL 2002 (6.2.3): refused
*> under -std=85, where a fixed-form program continues in column 7.
procedure division.
    display "one "-
    "two"
    stop run.
