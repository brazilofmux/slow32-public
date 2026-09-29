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

/* The order and combination rules of a numeric or numeric-edited
 * PICTURE (X3.23-1985 VI-33..37 and the precedence chart; 2023
 * 13.18.40.3 rules 12-29 and 13.18.40.6), over the flattened symbols
 * (CR and DB as C and D).  NULL when it is well formed. */
static const char *pic_rules(const char *f, int nf)
{
    int n[128] = { 0 };
    for (int i = 0; i < nf; i++) n[(unsigned char)f[i]]++;
    int ncrdb = n['C'] + n['D'];
    int edit = n['Z'] + n['*'] + n['+'] + n['-'] + n['$'] + ncrdb + n['.'] + n[','] + n['B'] + n['0'] + n['/'];
    if (n['S'] > 1 || (n['S'] && f[0] != 'S')) return "S is the first symbol, and appears once (2023 13.18.40.3 rule 18)";
    if (n['S'] && edit) return "S is not a numeric-edited symbol: an edited sign is +, -, CR or DB (2023 13.18.40.4 rule 13)";
    if (n['V'] > 1) return "V appears once (2023 13.18.40.3 rule 12b)";
    if (n['.'] > 1) return "the decimal point appears once (2023 13.18.40.3 rule 12b)";
    if (ncrdb > 1) return "CR or DB appears once (2023 13.18.40.3 rule 12b)";
    if (n['V'] && n['.']) return "V and the decimal point exclude each other (2023 13.18.40.3 rule 20)";
    if (n['P'] && n['.']) return "P and the decimal point exclude each other (2023 13.18.40.3 rule 17)";
    if (n['Z'] && n['*']) return "Z and * exclude each other (2023 13.18.40.3 rule 21)";
    if ((n['+'] && n['-']) || ((n['+'] || n['-']) && ncrdb))
        return "+, -, CR and DB exclude each other (2023 13.18.40.3 rule 23)";
    if (ncrdb && f[nf - 1] != 'C' && f[nf - 1] != 'D') return "CR or DB is the last symbol (the precedence rules, 2023 13.18.40.6)";
    /* one zero-suppression or floating string at most (rule 27) */
    int kinds = (n['+'] > 1) + (n['-'] > 1) + (n['$'] > 1) + (n['*'] > 0) + (n['Z'] > 0);
    if (kinds > 1) return "one zero-suppression or floating insertion string at most (2023 13.18.40.3 rule 27)";
    char F = n['Z'] ? 'Z' : n['*'] ? '*' : n['+'] > 1 ? '+' : n['-'] > 1 ? '-' : n['$'] > 1 ? '$' : 0;
    /* a fixed sign: the first or the last symbol (rule 25) */
    for (int k = 0; k < 2; k++) {
        char c = "+-"[k];
        if (n[(unsigned char)c] == 1)
            for (int i = 1; i < nf - 1; i++)
                if (f[i] == c) return "a fixed + or - is the first or the last symbol (2023 13.18.40.3 rule 25)";
    }
    /* a fixed currency symbol: first, or second after a fixed sign; or
     * last, or next to last before +, -, CR or DB (rule 26) */
    if (n['$'] == 1) {
        int i = 0; while (f[i] != '$') i++;
        int ok = i == 0 || (i == 1 && (f[0] == '+' || f[0] == '-')) || i == nf - 1 ||
                 (i == nf - 2 && strchr("+-CD", f[nf - 1]));
        if (!ok) return "a fixed currency symbol is at the left end or the right end (2023 13.18.40.3 rule 26)";
    }
    int dp = -1;
    for (int i = 0; i < nf; i++) if (f[i] == '.' || f[i] == 'V') { dp = i; break; }
    if (F) {
        int first = -1, last = -1;
        for (int i = 0; i < nf; i++) if (f[i] == F) { if (first < 0) first = i; last = i; }
        for (int i = 0; i < last; i++)
            if (f[i] == '9') return "a 9 cannot precede a zero-suppression or floating insertion symbol (the precedence rules, 2023 13.18.40.6)";
        for (int i = first; i <= last; i++)
            if (f[i] != F && !strchr("B0/,.V", f[i]))
                return "a zero-suppression or floating insertion string is broken by another symbol (the precedence rules, 2023 13.18.40.6)";
        if (dp >= 0 && last > dp && n['9'])
            return "past the decimal point a zero-suppression or floating string takes every digit position (2023 13.18.40.5, the editing rules)";
        if (F != 'Z' && F != '*' && dp >= 0 && first > dp)
            return "a floating insertion string starts left of the decimal point (2023 13.18.40.3 rule 29)";
    }
    /* P: one run, at the left or right end of the digit positions, V
     * beside it (2023 13.18.40.3 rules 16 and 19) */
    if (n['P']) {
        int first = -1, last = -1;
        for (int i = 0; i < nf; i++) if (f[i] == 'P') { if (first < 0) first = i; last = i; }
        for (int i = first; i <= last; i++) if (f[i] != 'P') return "P is one continuous string (2023 13.18.40.3 rule 16)";
        int digit_before = 0, digit_after = 0;
        for (int i = 0; i < first; i++) if (strchr("9Z*", f[i]) || (F && f[i] == F)) digit_before = 1;
        for (int i = last + 1; i < nf; i++) if (strchr("9Z*", f[i]) || (F && f[i] == F)) digit_after = 1;
        if (digit_before && digit_after) return "P is at the leftmost or the rightmost digit positions (2023 13.18.40.3 rule 16)";
        if (n['V']) {
            int v = 0; while (f[v] != 'V') v++;
            if (v != first - 1 && v != last + 1) return "V is next to the string of P (2023 13.18.40.3 rule 19)";
        }
    }
    return NULL;
}

/* Table 10 of 2023 13.18.40.6 (the precedence chart X3.23-1985 VI-37
 * carries too, less E, 1 and N): for each second symbol, the first
 * symbols that may precede it anywhere in the string.  Symbols with two
 * uses have two entries: a sign or currency symbol fixed at the left or
 * the right end; Z and * and floating strings left or right of the decimal
 * point; P left of it (99PP) or right of it (VPP99). */
enum { K_B, K_COMMA, K_POINT, K_SL, K_ST, K_CRDB, K_CSL, K_CST, K_ZL, K_ZR, K_FL, K_FR,
       K_CFL, K_CFR, K_9, K_AX, K_S, K_V, K_PL, K_PR, K_N };
#define B_(k) (1u << (k))
static const unsigned pic_chart[K_N] = {
    [K_B]     = B_(K_B)|B_(K_COMMA)|B_(K_POINT)|B_(K_SL)|B_(K_CSL)|B_(K_ZL)|B_(K_ZR)|B_(K_FL)|B_(K_FR)|B_(K_CFL)|B_(K_CFR)|B_(K_9)|B_(K_AX)|B_(K_V)|B_(K_PR),
    [K_COMMA] = B_(K_B)|B_(K_COMMA)|B_(K_POINT)|B_(K_SL)|B_(K_CSL)|B_(K_ZL)|B_(K_ZR)|B_(K_FL)|B_(K_FR)|B_(K_CFL)|B_(K_CFR)|B_(K_9)|B_(K_V)|B_(K_PR),
    [K_POINT] = B_(K_B)|B_(K_COMMA)|B_(K_SL)|B_(K_CSL)|B_(K_ZL)|B_(K_FL)|B_(K_CFL)|B_(K_9),
    [K_SL]    = 0,
    [K_ST]    = B_(K_B)|B_(K_COMMA)|B_(K_POINT)|B_(K_CSL)|B_(K_CST)|B_(K_ZL)|B_(K_ZR)|B_(K_CFL)|B_(K_CFR)|B_(K_9)|B_(K_V)|B_(K_PL)|B_(K_PR),
    [K_CRDB]  = B_(K_B)|B_(K_COMMA)|B_(K_POINT)|B_(K_CSL)|B_(K_CST)|B_(K_ZL)|B_(K_ZR)|B_(K_CFL)|B_(K_CFR)|B_(K_9)|B_(K_V)|B_(K_PL)|B_(K_PR),
    [K_CSL]   = B_(K_SL),
    [K_CST]   = B_(K_B)|B_(K_COMMA)|B_(K_POINT)|B_(K_SL)|B_(K_ZL)|B_(K_ZR)|B_(K_9)|B_(K_V)|B_(K_PL)|B_(K_PR),
    [K_ZL]    = B_(K_B)|B_(K_COMMA)|B_(K_SL)|B_(K_CSL)|B_(K_ZL),
    [K_ZR]    = B_(K_B)|B_(K_COMMA)|B_(K_POINT)|B_(K_SL)|B_(K_CSL)|B_(K_ZL)|B_(K_ZR)|B_(K_V)|B_(K_PR),
    [K_FL]    = B_(K_B)|B_(K_COMMA)|B_(K_CSL)|B_(K_FL),
    [K_FR]    = B_(K_B)|B_(K_COMMA)|B_(K_POINT)|B_(K_CSL)|B_(K_FL)|B_(K_FR)|B_(K_V),
    [K_CFL]   = B_(K_B)|B_(K_COMMA)|B_(K_SL)|B_(K_CFL),
    [K_CFR]   = B_(K_B)|B_(K_COMMA)|B_(K_POINT)|B_(K_SL)|B_(K_CFL)|B_(K_CFR)|B_(K_V),
    [K_9]     = B_(K_B)|B_(K_COMMA)|B_(K_POINT)|B_(K_SL)|B_(K_CSL)|B_(K_ZL)|B_(K_FL)|B_(K_CFL)|B_(K_9)|B_(K_AX)|B_(K_S)|B_(K_V)|B_(K_PR),
    [K_AX]    = B_(K_B)|B_(K_9)|B_(K_AX),
    [K_S]     = 0,
    [K_V]     = B_(K_B)|B_(K_COMMA)|B_(K_SL)|B_(K_CSL)|B_(K_ZL)|B_(K_FL)|B_(K_CFL)|B_(K_9)|B_(K_S)|B_(K_PL),
    [K_PL]    = B_(K_B)|B_(K_COMMA)|B_(K_SL)|B_(K_CSL)|B_(K_ZL)|B_(K_FL)|B_(K_CFL)|B_(K_9)|B_(K_S)|B_(K_PL),
    [K_PR]    = B_(K_SL)|B_(K_CSL)|B_(K_S)|B_(K_V)|B_(K_PR),
};
static const char *const pic_kname[K_N] = {
    "B, 0 or /", ",", ".", "a leading + or -", "a trailing + or -", "CR or DB", "a leading currency symbol",
    "a trailing currency symbol", "Z or * left of the point", "Z or * right of the point",
    "a floating + or - left of the point", "a floating + or - right of the point",
    "a floating currency symbol left of the point", "a floating currency symbol right of the point",
    "9", "A or X", "S", "V", "P left of the point", "P right of the point" };

static const char *pic_precedence(const char *f, int nf, char *msg, size_t msz)
{
    int n[128] = { 0 };
    for (int i = 0; i < nf; i++) n[(unsigned char)f[i]]++;
    int dp = -1;
    for (int i = 0; i < nf; i++) if (f[i] == '.' || f[i] == 'V') { dp = i; break; }
    /* P right of the point: a run at the leftmost digit positions */
    int pfirst = -1, pright = 0;
    for (int i = 0; i < nf; i++) if (f[i] == 'P') { pfirst = i; break; }
    if (pfirst >= 0) {
        pright = 1;
        for (int i = 0; i < pfirst; i++) if (strchr("9Z*AX", f[i]) || ((f[i] == '+' || f[i] == '-' || f[i] == '$') && n[(unsigned char)f[i]] > 1)) pright = 0;
        if (dp >= 0) pright = pfirst > dp;          /* after V (or the point) it is right of it, whatever precedes */
        int after = 0;
        for (int i = pfirst; i < nf; i++) if (strchr("9Z*", f[i]) || ((f[i] == '+' || f[i] == '-' || f[i] == '$') && n[(unsigned char)f[i]] > 1)) after = 1;
        if (!after && dp < 0) pright = 0;
    }
    int k[PIC_MAXPAT];
    for (int i = 0; i < nf; i++) {
        char c = f[i]; int right = dp >= 0 && i > dp;
        switch (c) {
        case 'B': case '0': case '/': k[i] = K_B; break;
        case ',': k[i] = K_COMMA; break;
        case '.': k[i] = K_POINT; break;
        case 'C': case 'D': k[i] = K_CRDB; break;
        case 'Z': case '*': k[i] = right ? K_ZR : K_ZL; break;
        case '+': case '-':
            k[i] = n[(unsigned char)c] > 1 ? (right ? K_FR : K_FL) : i == 0 ? K_SL : K_ST; break;
        case '$':
            k[i] = n['$'] > 1 ? (right ? K_CFR : K_CFL) : (i == 0 || (i == 1 && (f[0] == '+' || f[0] == '-'))) ? K_CSL : K_CST; break;
        case '9': k[i] = K_9; break;
        case 'A': case 'X': k[i] = K_AX; break;
        case 'S': k[i] = K_S; break;
        case 'V': k[i] = K_V; break;
        case 'P': k[i] = pright ? K_PR : K_PL; break;
        default: return NULL;
        }
    }
    for (int j = 1; j < nf; j++)
        for (int i = 0; i < j; i++)
            if (!(pic_chart[k[j]] & B_(k[i]))) {
                snprintf(msg, msz, "%s cannot follow %s (the precedence rules, 2023 13.18.40.6 Table 10)", pic_kname[k[j]], pic_kname[k[i]]);
                return msg;
            }
    /* at least one of A, X, Z, 9 or *, or two of +, - or the currency
     * symbol (2023 13.18.40.3 rule 12a; 85 5.9.6) */
    if (!(n['A'] + n['X'] + n['Z'] + n['9'] + n['*']) && n['+'] < 2 && n['-'] < 2 && n['$'] < 2) {
        snprintf(msg, msz, "it holds none of A, X, Z, 9 or *, nor two of +, - or the currency symbol (2023 13.18.40.3 rule 12a)");
        return msg;
    }
    return NULL;
}
#undef B_

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
    char pmsg[120];
    const char *bad = pic_rules(f, nf);
    if (!bad) bad = pic_precedence(f, nf, pmsg, sizeof pmsg);
    if (bad) {
        snprintf(info->err, sizeof info->err, "PICTURE '%s': %s", s, bad);
        return -1;
    }

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
