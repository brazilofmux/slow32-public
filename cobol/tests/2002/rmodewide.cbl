*> ROUNDED MODE on 31-digit items (2023 14.7.4): the wide store
*> (docs/wide.md) takes the same modes as the narrow one -- ties to even
*> and toward the greater and the lesser, past 18 digits.  The oracle runs
*> in GnuCOBOL's default dialect (its -std=cobol2002 has no ROUNDED MODE).
identification division.
program-id. rmodewide.
data division.
working-storage section.
01 a      pic s9(25)v9(5) value  1234567890123456789012345.50000.
01 b      pic s9(25)v9(5) value -1234567890123456789012344.50000.
01 c      pic s9(25)v9(5) value  1234567890123456789012345.00001.
01 r      pic s9(25).
procedure division.
    compute r rounded mode nearest-even = a
    display "ne  a " r
    compute r rounded mode nearest-even = b
    display "ne  b " r
    compute r rounded mode toward-greater = b
    display "tg  b " r
    compute r rounded mode toward-lesser = c
    display "tl  c " r
    compute r rounded mode away-from-zero = c
    display "afz c " r
    compute r rounded mode nearest-toward-zero = a
    display "ntz a " r
    stop run.
