/* The date, time and combined date-and-time formats of the 2014
 * international date and time functions (2023 15.3.1-15.3.3; ISO 8601):
 * shared by the compiler, which checks a format literal and sizes the
 * result (operand_parse.h), and the runtime, which renders and scans
 * data by one (libcob.c).  A part of whichever translation unit
 * includes it. */

typedef struct {
    int kind;       /* 1 a date format, 2 a time format, 3 combined (date T time) */
    int ext;        /* extended (hyphens, colons in the data), else basic */
    int dkind;      /* the date: 1 calendar YYYYMMDD, 2 ordinal YYYYDDD, 3 week YYYYWwwD */
    int frac;       /* the seconds' fraction digits, 0 for integer seconds */
    int tz;         /* the time: 0 local, 1 UTC (Z), 2 with an offset (+hhmm) */
    int len;        /* the data's length in characters (the basic fractional separator is not in the data) */
} cob_dtfmt;

/* f[0..n): the format; comma: DECIMAL-POINT IS COMMA, the fraction's
 * separator in the format and the extended data.  0 when it is a
 * format; otherwise the 1-based position of the first character that
 * is not */
static int cob_dtfmt_parse(const unsigned char *f, int n, int comma, cob_dtfmt *o)
{
    int i = 0;
    memset(o, 0, sizeof *o);
    #define LIT(s) do { const char *q = (s); while (*q) { if (i >= n || f[i] != (unsigned char)*q) return i + 1; i++; q++; } } while (0)
    /* the date part: four Y, then what follows */
    if (i < n && f[i] == 'Y') {
        LIT("YYYY");
        int ext = i < n && f[i] == '-';
        if (ext) i++;
        if (i < n && f[i] == 'M') { LIT("MM"); if (ext) LIT("-"); LIT("DD"); o->dkind = 1; o->len = ext ? 10 : 8; }
        else if (i < n && f[i] == 'W') { LIT("Www"); if (ext) LIT("-"); LIT("D"); o->dkind = 3; o->len = ext ? 10 : 8; }
        else if (i < n && f[i] == 'D') { LIT("DDD"); o->dkind = 2; o->len = ext ? 8 : 7; }
        else return i + 1;
        o->kind = 1; o->ext = ext;
        if (i == n) return 0;
        LIT("T");
        o->kind = 3; o->len += 1;
        if (i < n && (f[i] == '-' || f[i] == ':')) return i + 1;
    }
    /* the time part */
    if (i >= n || f[i] != 'h') return i + 1;
    LIT("hh");
    int ext = i < n && f[i] == ':';
    if (o->kind == 3 && ext != o->ext) return i + 1;      /* a basic date with an extended time, or the reverse (15.3.3.7) */
    if (ext) i++;
    LIT("mm"); if (ext) LIT(":"); LIT("ss");
    o->kind |= 2; o->ext = ext; o->len += ext ? 8 : 6;
    if (i < n && f[i] == (comma ? ',' : '.')) {
        i++;
        int k = 0;
        while (i < n && f[i] == 's') { i++; k++; }
        if (!k) return i + 1;
        o->frac = k; o->len += k + ext;                  /* the separator is in the extended data only */
    }
    if (i < n && f[i] == 'Z') { i++; o->tz = 1; o->len += 1; }
    else if (i < n && f[i] == '+') {
        i++; LIT("hh"); if (ext) LIT(":"); LIT("mm");
        o->tz = 2; o->len += ext ? 6 : 5;
    }
    #undef LIT
    return i == n ? 0 : i + 1;
}
