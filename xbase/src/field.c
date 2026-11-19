#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "field.h"
#include "util.h"
#include "date.h"
#include "command.h"
#include "codepage.h"

/* A character field's value: field_len characters (codepage.h), cut or
 * padded with blanks -- in CP437 field_len bytes, as ever.  In UTF-8 the
 * bytes are the table's to settle (dbf_set_field_raw converts and cuts on
 * a character); buf holds 256, so never past 255 bytes, nor through a
 * character. */
int field_format_char(char *buf, int field_len, const char *value) {
    int n = (int)strlen(value), b, chars, o;
    b = cp_offset(value, n, field_len);
    if (b > 255) {
        b = 255;
        while (b > 0 && ((unsigned char)value[b] & 0xC0) == 0x80) b--;
    }
    memcpy(buf, value, (size_t)b);
    o = b;
    chars = cp_chars(value, b);
    while (chars < field_len && o < 255) { buf[o++] = ' '; chars++; }
    buf[o] = '\0';
    return 0;
}

int field_format_numeric(char *buf, int field_len, int decimals, const char *value) {
    double v;
    int len;
    char tmp[64];

    v = atof(value);
    if (value[0] == '*' || !isfinite(v)) v = 1e300;   /* an overflow stays one: asterisks below */
    /* decimals==0 used (int)v, which wraps at 2³¹ on ILP32. N width is
     * up to 19; %.0f keeps values that fit in double. */
    snprintf(tmp, sizeof(tmp), "%*.*f", field_len, decimals > 0 ? decimals : 0, v);

    len = strlen(tmp);
    if (len > field_len) {
        /* Overflow: fill with asterisks */
        int i;
        for (i = 0; i < field_len; i++)
            buf[i] = '*';
        buf[field_len] = '\0';
        return 0;
    }
    /* Right-justify, space pad */
    if (len < field_len) {
        int pad = field_len - len;
        int i;
        for (i = 0; i < pad; i++)
            buf[i] = ' ';
        memcpy(buf + pad, tmp, len);
    } else {
        memcpy(buf, tmp, len);
    }
    buf[field_len] = '\0';
    return 0;
}

int field_format_date(char *buf, const char *value) {
    /* Input: date string in current SET DATE format, Output: YYYYMMDD (8 bytes) */
    int32_t jdn = date_from_display(value, cmd_get_date_format(), cmd_get_epoch());
    date_to_dbf(jdn, buf);
    return 0;
}

int field_format_logical(char *buf, const char *value) {
    const char *p = value;
    while (*p == ' ' || *p == '.') p++;
    if (*p == 'T' || *p == 't' || *p == 'Y' || *p == 'y')
        buf[0] = 'T';
    else
        buf[0] = 'F';
    buf[1] = '\0';
    return 0;
}

void field_display_char(char *buf, const char *raw, int field_len) {
    memcpy(buf, raw, field_len);
    buf[field_len] = '\0';
}

void field_display_numeric(char *buf, const char *raw, int field_len) {
    memcpy(buf, raw, field_len);
    buf[field_len] = '\0';
}

void field_display_date(char *buf, const char *raw) {
    /* Input: YYYYMMDD, Output: format-aware date string */
    int32_t jdn = date_from_dbf(raw);
    date_to_display(jdn, buf, cmd_get_date_format(), cmd_get_century(), cmd_get_mark());
}

void field_display_logical(char *buf, const char *raw) {
    if (raw[0] == 'T' || raw[0] == 't')
        strcpy(buf, ".T.");
    else
        strcpy(buf, ".F.");
}
