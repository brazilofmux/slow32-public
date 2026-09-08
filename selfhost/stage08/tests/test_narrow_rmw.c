/* GitHub issue 43: compound assign and ++/-- on a promoted char/short
 * must wrap.  Return-site hl_narrow hid sqlite3StrIHash (the function
 * returned 5443); a compare of the local after += still saw the wide
 * value.  Values come from volatile so constant-folding cannot wrap
 * them at compile time.  Own file because test_sqlite_bugs.c's bitmask
 * already uses every 8-bit exit bit (4096 & 255 == 0). */
static volatile unsigned char vu8;
static volatile unsigned short vu16;
static volatile signed char vs8;

int main(void) {
    unsigned char h;
    unsigned short s;
    signed char c;

    vu8 = 250;
    h = vu8;
    h += 10;
    if (h != 4) return 1;
    if (!(h < 10)) return 2;

    vu8 = 255;
    h = vu8;
    h++;
    if (h != 0) return 3;

    vu8 = 0;
    h = vu8;
    h--;
    if (h != 255) return 4;

    vu16 = 65530;
    s = vu16;
    s += 10;
    if (s != 4) return 5;

    vs8 = 120;
    c = vs8;
    c += 20;
    if (c != (signed char)140) return 6;

    vu8 = 255;
    h = vu8;
    if (h++ != 255) return 7;
    if (h != 0) return 8;

    vu8 = 255;
    h = vu8;
    if ((++h) != 0) return 9;

    return 0;
}
