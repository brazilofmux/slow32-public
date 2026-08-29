/* The C side of tests/fixed/cbridge.cbl: the shapes dateutil.c has --
 * scalars BY VALUE, a struct of shorts BY REFERENCE, an int through a
 * pointer, a bool back in the return register. */
typedef struct {
             short year;
    unsigned short month;
    unsigned short dayofweek;
    unsigned short dayofmonth;
    unsigned short dayofyear;
} FIELDED;

int bt_fill(int ld, FIELDED *fd)
{
    if (ld < 0) return 0;
    fd->year = (short)(1601 + ld / 365);
    fd->month = (unsigned short)(1 + (ld / 31) % 12);
    fd->dayofweek = (unsigned short)(ld % 7);
    fd->dayofmonth = (unsigned short)(1 + ld % 28);
    fd->dayofyear = (unsigned short)(1 + ld % 365);
    return 1;
}

int bt_pack(short y, unsigned short m, unsigned short d, int *out)
{
    if (m < 1 || m > 12 || d < 1 || d > 31) return 0;
    *out = y * 10000 + m * 100 + d;
    return 1;
}

int bt_neg(int x) { return -x; }
