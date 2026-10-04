/* scrnum_test.c -- the screen editor's core for numeric fields, on the
 * host.
 *
 *   scrnum_test PICTURE VALUE KEYS
 *
 * prints the field and the cursor after each key, in the form
 * tests/adischeck.sh prints what Micro Focus's ADIS does with the same
 * picture and keys (tests/scredit-differential.sh -N compares the two).
 * VALUE is [-]digits[.digits]; KEYS as adischeck's.  The field is drawn
 * the way ADIS draws it, so that every key can be compared: with its
 * prompt character, and -- what the runtime does not copy -- a picture
 * with no point drawn one position to the left while the cursor is past
 * its digits.
 *
 * N:PICTURE is the same picture under natural entry, which is how the
 * runtime keys every numeric field (sn_natural; not ADIS's behaviour). */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "picture.h"
#include "kern.h"
#include "scredit.h"

static sn_field F; static sn_state S; static PicInfo PI; static int fresh;

static void image(char *out, int cur)
{
    char digs[2 * SN_MAXD]; memset(digs, '0', sizeof digs);
    sn_digits(&F, &S, digs);
    memset(out, ' ', SN_MAXW);
    /* ADIS, in a picture with no point, draws the digits one position to
     * the left while the cursor is past them, the last column the slot
     * the next digit goes into.  (Not something the runtime copies: it is
     * here so the differential can compare every key.) */
    int slot = cur && !F.natural && !S.frac && S.pos == F.ni && F.pcol < 0 && S.lead > 0 && F.first9 > 0;
    int lead = S.lead;
    if (slot) { memmove(digs, digs + 1, (size_t)(F.ni - 1)); digs[F.ni - 1] = '0'; lead--; }
    /* a zero at the first shown position is data: the editing code is
     * given a 1 there and the column patched back */
    int fs = 0; while (fs < F.ni && digs[fs] == '0') fs++;
    int patch = F.edited && lead < F.ni && fs > lead;
    if (patch) digs[lead] = '1';
    if (F.edited) cob_edit_apply(F.pat, digs, S.neg, 0, out);
    else memcpy(out, digs, (size_t)(F.ni + F.nf));
    if (patch) out[F.icol[lead]] = '0';
    if (slot) out[F.icol[F.ni - 1]] = ' ';
    if (cur) {
        /* while the field is the current one: the fraction and the point
         * always show, and a blank before the last integer position is
         * the prompt character */
        if (F.pcol >= 0) out[F.pcol] = '.';
        for (int j = 0; j < F.nf; j++) out[F.fcol[j]] = S.fd[j];
        int lim = F.pcol >= 0 ? F.pcol : F.ni ? F.icol[F.ni - 1] + 1 : 0;
        for (int c = lim - 1; c >= 0; c--)
            {
            if (out[c] != ' ') continue;
            /* a fixed sign's column only when the next is a prompt; the
             * floating string's first unless a digit stands next to it */
            if (F.kind[c] == 's' && !(c + 1 < lim && out[c + 1] == '_')) continue;
            if (F.kind[c] == 'F' && c + 1 < lim && out[c + 1] >= '0' && out[c + 1] <= '9') continue;
            out[c] = '_';
        }
    }
    out[F.width] = 0;
}
static void show(const char *key) { char img[SN_MAXW + 1]; image(img, 1); printf("%-8s [%s]  %d\n", key, img, sn_cursor(&F, &S) + 1); }

int main(int argc, char **argv)
{
    if (argc != 4) { fprintf(stderr, "usage: scrnum_test PICTURE VALUE KEYS\n"); return 2; }
    int natural = !strncmp(argv[1], "N:", 2);            /* N:PICTURE: natural entry, as the runtime keys it */
    if (natural) argv[1] += 2;
    if (pic_analyse(argv[1], &PI)) { fprintf(stderr, "scrnum_test: %s\n", PI.err); return 2; }
    if (sn_field_init(&F, PI.pat, PI.floating, PI.category == PIC_NUMERIC_EDITED)) { fprintf(stderr, "scrnum_test: not handled: %s\n", argv[1]); return 2; }
    /* VALUE: [-]int[.frac] */
    char digs[2 * SN_MAXD]; memset(digs, '0', sizeof digs);
    const char *v = argv[2]; int neg = 0;
    if (*v == '-') { neg = 1; v++; }
    const char *dot = strchr(v, '.'); int il = dot ? (int)(dot - v) : (int)strlen(v);
    for (int k = 0; k < il && k < F.ni; k++) digs[F.ni - 1 - k] = v[il - 1 - k];
    if (dot) for (int k = 0; k < F.nf && dot[1 + k]; k++) digs[F.ni + k] = dot[1 + k];
    if (natural) sn_natural(&F);
    sn_enter(&F, &S, digs, neg);
    if (natural) {                                       /* libcob's scr_num_start: on the point, the first key replacing */
        S.frac = 0; S.pos = F.ni; S.off_end = 0;
        if (F.nf == 0 && F.ni && S.lead == 0) { S.pos = F.ni - 1; S.off_end = 1; }
        fresh = 1;
    }
    show("(start)");
    for (const char *p = argv[3]; *p; ) {
        char name[16]; int fn = 0, ch = 0;
        if (*p == '{') {
            const char *q = strchr(p, '}'); if (!q) break;
            snprintf(name, sizeof name, "%.*s", (int)(q - p - 1), p + 1); p = q + 1;
            if (!strcmp(name, "ENTER")) {
                char d[2 * SN_MAXD]; sn_digits(&F, &S, d);
                int n = F.ni + F.nf;
                if (S.neg && n) d[n - 1] = (char)('p' + (d[n - 1] - '0'));
                printf("ITEM=[%.*s]\n", n, d);
                return 0;
            }
            fn = !strcmp(name, "LEFT") ? SN_LEFT : !strcmp(name, "RIGHT") ? SN_RIGHT : !strcmp(name, "END") ? SN_END
               : !strcmp(name, "BS") ? SN_BACKSPACE : !strcmp(name, "DEL") ? SN_DELETE
               : !strcmp(name, "^X") ? SN_CLEAR_FIELD : !strcmp(name, "^Z") ? SN_CLEAR_EOF : !strcmp(name, "^A") ? SN_UNDO
               : !strcmp(name, "HOME") ? -1 : !strcmp(name, "INS") ? -2 : 0;
            if (!fn) { fprintf(stderr, "scrnum_test: what is {%s}?\n", name); return 2; }
        } else {
            name[0] = *p; name[1] = 0; ch = (unsigned char)*p++;
            fn = ch >= '0' && ch <= '9' ? SN_DIGIT : ch == '.' ? SN_POINT : ch == '-' ? SN_MINUS : ch == '+' ? SN_PLUS : -2;
        }
        if (fn == -1) sn_home(&F, &S);
        else if (fn != -2) {
            if (natural && ((fn == SN_POINT && !F.nf) || ((fn == SN_MINUS || fn == SN_PLUS) && !F.has_sign))) { show(name); continue; }
            if (fresh && (fn == SN_DIGIT || fn == SN_POINT || fn == SN_BACKSPACE)) sn_key(&F, &S, SN_CLEAR_FIELD, 0);
            if (fn != SN_MINUS && fn != SN_PLUS) fresh = 0;
            sn_key(&F, &S, fn, ch);
        }
        show(name);
    }
    printf("(keys ran out)\n");
    return 0;
}
