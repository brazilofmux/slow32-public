/* COBOL PICTURE analysis: turn the scanned symbols into a field description.
 *
 * Rewritten from cobc370's picture.c.  The category / digits / scale /
 * sign synthesis is the same reading of the standard (X3.23-1985 is
 * unchanged from 1974 here); the S/370 ED mask is gone.  What the runtime
 * gets instead is the flattened symbol string, `pat`, which is the whole
 * edit descriptor: a software editor walks it left to right with a
 * significance flag, exactly as ED would have.
 */
#include <stdio.h>
#include <string.h>
#include "picture.h"

static int fail(PicInfo *in, const char *msg)
{
    snprintf(in->err, sizeof in->err, "%s", msg);
    return -1;
}

const char *pic_category_name(int c)
{
    switch (c) {
    case PIC_ALPHABETIC:          return "alphabetic";
    case PIC_ALPHANUMERIC:        return "alphanumeric";
    case PIC_ALPHANUMERIC_EDITED: return "alphanumeric-edited";
    case PIC_NUMERIC:             return "numeric";
    case PIC_NUMERIC_EDITED:      return "numeric-edited";
    }
    return "?";
}

int pic_analyse(const char *s, PicInfo *info)
{
    PicItem it[PIC_MAXITEM];
    int errpos = 0;
    memset(info, 0, sizeof *info);

    int n = pic_scan(s, it, PIC_MAXITEM, &errpos);
    if (n < 0) {
        snprintf(info->err, sizeof info->err,
                 "PICTURE '%s' is not valid at character %d", s, errpos + 1);
        return -1;
    }
    if (n == 0) return fail(info, "empty PICTURE");

    /* Flatten.  X(2100) is ordinary and the pattern is bounded, so an
     * alphanumeric picture wider than the pattern is recorded by width
     * only; nothing edits a plain X item anyway. */
    int total = 0, nx = 0, ins = 0, n9 = 0;
    for (int i = 0; i < n; i++) {
        total += it[i].rep;
        if (it[i].sym == 'X' || it[i].sym == 'A') nx++;
        if (it[i].sym == '9') n9++;
        if (it[i].sym == 'B' || it[i].sym == '0' || it[i].sym == '/') ins++;
    }

    if (nx) {
        /* Alphanumeric, possibly edited: A, X and 9 in any combination (all
         * A is alphabetic; 9 among them makes the item alphanumeric, X3.23
         * 5.3.9), joined by the simple insertion characters B, 0 and /,
         * which occupy positions of their own and are not filled from the
         * sending item. */
        if (nx + n9 + ins != n)
            return fail(info, "an alphanumeric PICTURE takes A, X, 9 and the insertions B, 0 and /");
        info->category = PIC_ALPHABETIC;
        for (int i = 0; i < n; i++)
            if (it[i].sym == 'X' || it[i].sym == '9') info->category = PIC_ALPHANUMERIC;
        info->bytes = total;
        if (ins) {
            if (total > PIC_MAXPAT - 1)
                return fail(info, "an edited alphanumeric item is too wide");
            info->category = PIC_ALPHANUMERIC_EDITED;
            info->edited = 1;
            for (int i = 0; i < n; i++)
                for (int r = 0; r < it[i].rep; r++)
                    info->pat[info->patlen++] = it[i].sym;
            info->pat[info->patlen] = 0;
        }
        return 0;
    }

    /* Numeric pictures are short by construction -- at most 18 digit
     * positions plus insertions -- so flattening is safe. */
    char f[PIC_MAXPAT];
    int nf = 0;
    for (int i = 0; i < n; i++)
        for (int r = 0; r < it[i].rep; r++) {
            if (nf >= PIC_MAXPAT - 1) return fail(info, "numeric PICTURE too long");
            f[nf++] = it[i].sym;
        }
    f[nf] = 0;

    /* A floating insertion string is the whole run of one sign or currency
     * symbol, and it is NOT broken by the insertion characters embedded in
     * it: ----,---,--9 is one floating string of nine '-', not three runs.
     * Nine symbols give eight digit positions and one sign position. */
    char fl = 0;
    int  fl_first = -1;
    for (int k = 0; k < 3; k++) {
        char c = "+-$"[k];
        int cnt = 0, first = -1;
        for (int i = 0; i < nf; i++)
            if (f[i] == c) { cnt++; if (first < 0) first = i; }
        if (cnt > 1) { fl = c; fl_first = first; break; }
    }

    int seen_point = 0, lead_p = 0, trail_p = 0, stored = 0;
    for (int i = 0; i < nf; i++) {
        char c = f[i];
        if (fl && c == fl) {
            info->edited = 1;
            info->bytes++;
            if (c != '$') info->is_signed = 1;
            if (i != fl_first) {            /* every symbol but the first is a digit */
                info->digits++; stored++;
                if (seen_point) info->scale++;
            }
            continue;
        }
        switch (c) {
        case '9': info->digits++; if (seen_point) info->scale++; info->bytes++;
                  stored++; break;
        /* P is an assumed decimal scaling position: it counts toward the
         * eighteen digits and toward the value's scale, but occupies no
         * character position.  A run of P's on the right multiplies the
         * stored digits; a run on the left makes them all fractional. */
        case 'P': info->digits++;
                  if (stored) trail_p++; else lead_p++;
                  break;
        case 'Z': info->digits++; if (seen_point) info->scale++; info->bytes++;
                  info->edited = 1; stored++; break;
        case '*': info->digits++; if (seen_point) info->scale++; info->bytes++;
                  info->edited = 1; stored++; break;
        case 'V': seen_point = 1; break;                  /* no character */
        case 'S': info->is_signed = 1; break;             /* no character */
        case '.': seen_point = 1; info->bytes++; info->edited = 1; break;
        case ',': case 'B': case '0': case '/':
                  info->bytes++; info->edited = 1; break;
        case '+': case '-':                                /* a fixed sign */
                  info->is_signed = 1; info->edited = 1; info->bytes++; break;
        case '$': info->edited = 1; info->bytes++; break;  /* fixed currency */
        case 'C': case 'D':                                /* CR / DB */
                  info->bytes += 2; info->edited = 1; info->is_signed = 1; break;
        default:  return fail(info, "unsupported PICTURE character");
        }
    }
    info->floating = fl;

    /* P beside Z, * or a floating string is an edited picture with scaling
     * positions (ZZZPP); the editor gives P no character */
    if (lead_p && trail_p)
        return fail(info, "P may run to the left or to the right, not both");
    if (trail_p) info->scale = -trail_p;
    if (lead_p)  info->scale = lead_p + stored;

    if (info->digits == 0) return fail(info, "PICTURE has no digit positions");
    /* The standard's ceiling: numeric literals and arithmetic operands are
     * 1 through 18 digits.  This machine could hold more; the language
     * does not. */
    if (info->digits > 18)
        return fail(info, "more than 18 digits -- the standard's limit for a "
                          "numeric item is 18");

    info->category = info->edited ? PIC_NUMERIC_EDITED : PIC_NUMERIC;
    memcpy(info->pat, f, nf + 1);
    info->patlen = nf;
    return 0;
}
