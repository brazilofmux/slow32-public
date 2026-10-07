identification division.
program-id. p-float-picture.
*> A standard floating-point usage takes no PICTURE (2023 13.18.40.3 rule
*> 1: the usages that take one; 13.18.60).
data division.
working-storage section.
01 f pic 9(5) usage float-binary-64.
procedure division.
    goback.
