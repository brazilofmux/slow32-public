/* The C side of tests/2002/callc.cbl: ten int arguments, the ninth and
 * tenth on the stack as the C ABI puts them, and a result in r1 for
 * CALL ... RETURNING under -std=2002 (a C function, not a COBOL program
 * with PROCEDURE DIVISION RETURNING). */
int weigh10(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j)
{
    return a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g + 8 * h + 9 * i + 10 * j;
}
