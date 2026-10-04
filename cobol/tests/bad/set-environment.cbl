identification division.
program-id. setenvbad.
*> SET ENVIRONMENT is GnuCOBOL's own (BP-G1): refused by default,
*> naming the switch that takes it -- GnuCOBOL's forms are never the
*> default (cobol ISSUES-124).
data division.
working-storage section.
01 v pic x(5).
procedure division.
    set environment "S32_X" to "Y".
    goback.
