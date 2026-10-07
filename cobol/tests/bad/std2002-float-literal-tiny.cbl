identification division.
program-id. p-float-literal-tiny.
*> A floating-point literal is held as a fixed-point value of at most 31
*> digits here (2023 8.3.3.3.3 rule 3, the exponent range the implementor's);
*> 1.0E-200 has 201 of them.  It overran the compiler's buffer once (the
*> zeros after the point were not counted).
data division.
working-storage section.
01 fl usage float-long value 1.0e-200.
procedure division.
    display fl
    stop run.
