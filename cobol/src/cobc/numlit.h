/* s32-cobc: numeric literals.  A part of one translation unit, included by
 * s32-cobc.c in order; not a header to include anywhere else. */

/* ====================================================================== */
/* Numeric literals                                                        */
/* ====================================================================== */

typedef struct {
    int neg;
    char digits[40];    /* all digits, no point, no leading sign */
    int ndigits, scale; /* scale = digits after the point */
} NumLit;

static void numlit_parse(Tok *t, NumLit *n)
{
    memset(n, 0, sizeof *n);
    const char *s = t->s;
    if (*s == '+' || *s == '-') { n->neg = (*s == '-'); s++; }
    int seen = 0;
    for (; *s; s++) {
        if (*s == '.') { seen = 1; continue; }
        if (n->ndigits >= 36) die_at(t->line, "numeric literal too long");
        n->digits[n->ndigits++] = *s;
        if (seen) n->scale++;
    }
    if (g_std >= 2002 ? n->ndigits > 31 : (n->ndigits - n->scale > 18 || n->ndigits > 36))
        die_at(t->line, g_std >= 2002 ? "numeric literal has more than 31 digits (2023 8.3.1.2.2.2 rule 1)" : "numeric literal has more than 18 digits");
}
/* a literal past the 64-bit path (COBOL 2002's 31 digits; docs/wide.md) */
static int numlit_wide(const NumLit *n) { return n->ndigits > 18; }

static void numlit_zero(NumLit *n) { memset(n, 0, sizeof *n); n->digits[0] = '0'; n->ndigits = 1; }

static int numlit_is_zero(const NumLit *n)
{
    for (int i = 0; i < n->ndigits; i++) if (n->digits[i] != '0') return 0;
    return 1;
}

static int numlit_is_int(const NumLit *n)
{
    for (int i = n->ndigits - n->scale; i < n->ndigits; i++) if (n->digits[i] != '0') return 0;
    return 1;
}

static long long numlit_int(const NumLit *n)       /* integer part; saturated past 18 digits, never wrapped */
{
    if (n->ndigits - n->scale > 18) return n->neg ? -9223372036854775807LL : 9223372036854775807LL;
    long long v = 0;
    for (int i = 0; i < n->ndigits - n->scale; i++) v = v * 10 + (n->digits[i] - '0');
    return n->neg ? -v : v;
}

static long long numlit_scaled(const NumLit *n)    /* all digits as an integer; saturated past 18 */
{
    if (n->ndigits > 18) return n->neg ? -9223372036854775807LL : 9223372036854775807LL;
    long long v = 0;
    for (int i = 0; i < n->ndigits; i++) v = v * 10 + (n->digits[i] - '0');
    return n->neg ? -v : v;
}

/* Value of the literal scaled to `scale` decimal places, as digit text
 * `out` of exactly `digits` characters (right-aligned, zero-filled).
 * Returns 0 if the integer part does not fit. */
static int numlit_align(const NumLit *n, int digits, int scale, char *out)
{
    int int_digits = n->ndigits - n->scale;
    int want_int = digits - scale;
    memset(out, '0', digits);
    for (int i = 0; i < int_digits; i++) {
        int pos = want_int - int_digits + i;
        if (pos < 0) { if (n->digits[i] != '0') return 0; continue; }
        out[pos] = n->digits[i];
    }
    for (int i = 0; i < n->scale && i < scale; i++)
        out[want_int + i] = n->digits[int_digits + i];
    return 1;
}
