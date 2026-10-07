identification division.
program-id. p-intermediate-bad.
*> INTERMEDIATE ROUNDING IS with a mode that is not one of its four (2023
*> 11.9.11: NEAREST-TOWARD-ZERO is ROUNDED's, not this clause's).
options.
    intermediate rounding is nearest-toward-zero.
procedure division.
    goback.
