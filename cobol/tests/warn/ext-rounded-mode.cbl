identification division.
program-id. extrmode.
*> -warn-extensions: ROUNDED MODE is COBOL 2014 (BP-E29).
data division.
working-storage section.
01 a pic s9v99 value 1.
procedure division.
    compute a rounded mode nearest-even = a / 3
    stop run.
