/* Double loads through every addressing shape: global array index,
 * pointer deref, struct member, pointer-to-struct member, and the
 * global-array initializer that feeds them (lexer decimal->binary +
 * FP init emission).  Regression test for the S12CC_NATIVE_F64 hole
 * where deref/member double loads fell into the 32-bit pair path and
 * read only the low word (found by the FP torture on x86-64). */
int printf(char *fmt, ...);

double vals[4] = {0.0, 1.5, -2.25, 3.14159265358979};

struct pair {
    int tag;
    double d;
};

struct pair gp = {7, 123456789.125};

static void show(char *label, double v)
{
    unsigned long long bits = *(unsigned long long *)&v;
    printf("%s %08x%08x\n", label, (unsigned)(bits >> 32), (unsigned)bits);
}

int main(void)
{
    int i;
    double *p = vals;
    struct pair loc;
    struct pair *pp = &gp;

    for (i = 0; i < 4; i++)
        show("idx", vals[i]);
    show("deref", *(p + 2));
    show("member", gp.d);
    show("ptrmem", pp->d);

    loc.tag = 1;
    loc.d = vals[3] * 2.0;
    show("locmem", loc.d);

    /* Indexed store then indexed read-back */
    vals[0] = vals[1] + vals[2];
    show("rmw", vals[0]);

    return (int)(gp.d / 1000000.0);
}
