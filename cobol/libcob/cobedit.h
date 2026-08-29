/* cobedit.h -- the software edit descriptor, applied and reversed.
 *
 * Included by libcob (guest, for MOVE to and from edited items) and by
 * s32-cobc (host, for a numeric VALUE on an edited item), so both edit
 * identically.  The descriptor is the flattened PICTURE symbol string that
 * picture.c produces: one character per position, CR/DB as 'C'/'D', with
 * V/S/P kept so the point is known.  cobc370 handed the same information
 * to the S/370 ED instruction as a mask; here it is walked in C.
 *
 * Rules are the 1985 PICTURE clause: zero suppression (Z, *), floating
 * insertion (+, -, $ -- n symbols give n-1 digit positions and the symbol
 * lands immediately left of the first significant character), simple
 * insertion (, B 0 /) replaced by the fill character while suppression is
 * in force, fixed signs (+ - CR DB), the decimal point ending suppression,
 * BLANK WHEN ZERO, and the all-suppressed zero.
 */
#ifndef COBEDIT_H
#define COBEDIT_H
#include <string.h>

static char cob_edit_floating(const char *pat)
{
    int cp = 0, cm = 0, cd = 0;
    for (const char *p = pat; *p; p++) { if (*p == '+') cp++; else if (*p == '-') cm++; else if (*p == '$') cd++; }
    return cp > 1 ? '+' : cm > 1 ? '-' : cd > 1 ? '$' : 0;
}

/* Edit the digit string digs (exactly the picture's digit positions,
 * integer part then fraction) into out.  Returns the number of bytes
 * written (the item's width). */
static int cob_edit_apply(const char *pat, const char *digs, int neg, int blank_zero, char *out)
{
    char fl = cob_edit_floating(pat);
    int fl_seen = 0, flpos = -1;
    char fill = strchr(pat, '*') ? '*' : ' ';
    int zero = 1, has9 = 0, npos = 0;
    for (const char *p = pat; *p; p++) {
        if (*p == '9') has9 = 1;
        if (*p == '9' || *p == 'Z' || *p == '*' || (fl && *p == fl)) npos++;
    }
    if (fl) npos--;
    for (int i = 0; i < npos; i++) if (digs[i] != '0') { zero = 0; break; }

    int sig = 0, di = 0, o = 0, first_sig = -1;
    for (const char *p = pat; *p; p++) {
        char c = *p;
        if (fl && c == fl) {
            if (!fl_seen) { fl_seen = 1; flpos = o; out[o++] = fill; continue; }
            char d = digs[di++];
            if (d != '0' || sig) { if (!sig) { sig = 1; first_sig = o; } out[o++] = d; }
            else out[o++] = fill;
            continue;
        }
        switch (c) {
        case '9': { char d = digs[di++]; if (!sig) { sig = 1; first_sig = o; } out[o++] = d; break; }
        case 'Z': case '*': {
            char d = digs[di++];
            if (d != '0' || sig) { if (!sig) { sig = 1; first_sig = o; } out[o++] = d; }
            else out[o++] = fill;
            break;
        }
        case '.': if (!sig) { sig = 1; first_sig = o; } out[o++] = '.'; break;
        case ',': out[o++] = sig ? ',' : fill; break;
        case 'B': out[o++] = sig ? ' ' : fill; break;
        case '0': out[o++] = sig ? '0' : fill; break;
        case '/': out[o++] = sig ? '/' : fill; break;
        case '+': out[o++] = neg ? '-' : '+'; break;
        case '-': out[o++] = neg ? '-' : ' '; break;
        case '$': out[o++] = '$'; break;
        case 'C': out[o++] = neg ? 'C' : ' '; out[o++] = neg ? 'R' : ' '; break;
        case 'D': out[o++] = neg ? 'D' : ' '; out[o++] = neg ? 'B' : ' '; break;
        default: break;                          /* V S P: no character */
        }
    }
    int width = o;

    if (blank_zero && zero) { memset(out, ' ', width); return width; }

    if (fl) {
        if (first_sig < 0) {
            /* every digit position floats and the value is zero: spaces */
            memset(out, ' ', width);
        } else {
            int pos = first_sig - 1;
            if (pos < flpos) pos = flpos;
            out[pos] = fl == '$' ? '$' : fl == '+' ? (neg ? '-' : '+') : (neg ? '-' : ' ');
        }
        return width;
    }
    if (zero && !has9) {
        /* all Z or all *: spaces throughout, or asterisks around the point */
        for (int i = 0; i < width; i++)
            if (fill == ' ' || out[i] != '.') out[i] = fill == ' ' ? ' ' : (out[i] == '.' ? '.' : '*');
        if (fill == '*') {
            /* keep fixed sign / currency / CR DB positions as edited */
            int k = 0;
            for (const char *p = pat; *p; p++) {
                if (*p == '+' || *p == '-' || *p == '$') { out[k] = (*p == '$') ? '$' : (*p == '+' ? '+' : ' '); k++; }
                else if (*p == 'C' || *p == 'D') { out[k] = out[k + 1] = ' '; k += 2; }
                else if (*p != 'V' && *p != 'S' && *p != 'P') k++;
            }
        }
    }
    return width;
}

/* Reverse: the digits and sign of an edited item.  digs receives exactly
 * the picture's digit positions. Returns the digit count. */
static int cob_deedit(const char *pat, const unsigned char *bytes, char *digs, int *neg)
{
    char fl = cob_edit_floating(pat);
    int fl_seen = 0, bi = 0, di = 0, minus = 0;
    for (const char *p = pat; *p; p++) {
        char c = *p;
        if (fl && c == fl) {
            if (!fl_seen) { fl_seen = 1; if (bytes[bi] == '-') minus = 1; bi++; continue; }
            unsigned char b = bytes[bi++];
            if (b == '-') minus = 1;
            digs[di++] = (b >= '0' && b <= '9') ? (char)b : '0';
            continue;
        }
        switch (c) {
        case '9': case 'Z': case '*': {
            unsigned char b = bytes[bi++];
            digs[di++] = (b >= '0' && b <= '9') ? (char)b : '0';
            break;
        }
        case '+': case '-': if (bytes[bi] == '-') minus = 1; bi++; break;
        case 'C': case 'D': if (bytes[bi] == 'C' || bytes[bi] == 'D') minus = 1; bi += 2; break;
        case '.': case ',': case 'B': case '0': case '/': case '$':
            if (bytes[bi] == '-') minus = 1;
            bi++; break;
        default: break;
        }
    }
    *neg = minus;
    return di;
}

#endif
