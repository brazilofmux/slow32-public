identification division.
program-id. cnoas.
*> -warn-extensions under -std=2002: a constant entry without AS is
*> GnuCOBOL's (BP-E25); two X-COBOL programs write it.
data division.
working-storage section.
01 width constant 20.
procedure division.
    stop run.
