identification division.
program-id. extvtr.
*> -warn-extensions under -dialect=mf: a VALUE literal cut (BP-D7); the
*> cut is warned without the flag too.
data division.
working-storage section.
01 m pic x(3) value "abcd".
procedure division.
    stop run.
