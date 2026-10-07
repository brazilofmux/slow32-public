identification division.
program-id. epr.
*> RAISING LAST EXCEPTION belongs in a declarative procedure or a WHEN
*> phrase (2023 14.9.18.3 rule 5, 14.9.14.3).
procedure division.
    exit program raising last exception.
