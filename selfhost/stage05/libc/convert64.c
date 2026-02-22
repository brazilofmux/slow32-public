/* Selfhost libc: 64-bit conversion functions
 *
 * Separate file because stage04's compiler cannot handle long long.
 * Only compiled by gen1 (stage05 s12cc) in the gen1 libc recompile step.
 */

static void rev_str(char *start, char *end) {
    char tmp;
    while (start < end) {
        tmp = *start;
        *start = *end;
        *end = tmp;
        start = start + 1;
        end = end - 1;
    }
}

int slow32_utoa64(unsigned long long val, char *buf) {
    int i;
    int d;
    if (val == 0) {
        buf[0] = '0';
        buf[1] = 0;
        return 1;
    }
    i = 0;
    while (val > 0) {
        d = (int)(val % 10);
        buf[i] = '0' + (char)d;
        val = val / 10;
        i = i + 1;
    }
    buf[i] = 0;
    rev_str(buf, buf + i - 1);
    return i;
}

int slow32_ltoa64(long long val, char *buf) {
    char *p;
    unsigned long long uval;
    int len;
    p = buf;
    if (val < 0) {
        *p = '-';
        p = p + 1;
        uval = (unsigned long long)(0 - val);
    } else {
        uval = (unsigned long long)val;
    }
    len = slow32_utoa64(uval, p);
    return (int)(p - buf) + len;
}

int slow32_utox64(unsigned long long val, char *buf, int upper) {
    int i;
    int d;
    if (val == 0) {
        buf[0] = '0';
        buf[1] = 0;
        return 1;
    }
    i = 0;
    while (val > 0) {
        d = (int)(val & 15);
        if (d < 10) buf[i] = '0' + (char)d;
        else buf[i] = (upper ? 'A' : 'a') + (char)(d - 10);
        val = val >> 4;
        i = i + 1;
    }
    buf[i] = 0;
    rev_str(buf, buf + i - 1);
    return i;
}

int slow32_utoo64(unsigned long long val, char *buf) {
    int i;
    int d;
    if (val == 0) {
        buf[0] = '0';
        buf[1] = 0;
        return 1;
    }
    i = 0;
    while (val > 0) {
        d = (int)(val & 7);
        buf[i] = '0' + (char)d;
        val = val >> 3;
        i = i + 1;
    }
    buf[i] = 0;
    rev_str(buf, buf + i - 1);
    return i;
}
