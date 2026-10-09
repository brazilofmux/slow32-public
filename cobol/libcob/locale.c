/* locale.c -- the runtime's locales: the record, the current locale, and
 * locale-based comparison (docs/plans/locale.md; 2023 8.2, 8.8.4.2.11,
 * 14.6.6, 15.51, 15.85).
 *
 * Built into libcobloc.s32a with the libutf units under libcob/utf, and
 * linked by compile.sh only into a program whose text names a cob_loc_
 * entry: the collation tables are 650 KB, and the programs that never
 * compare under a locale should not carry them.  Nothing here is called
 * from libcob.c, for that reason.
 *
 * Alphanumeric data is UTF-8 and national data UTF-16BE (docs/national.md);
 * libutf takes UTF-8, so national operands are converted on the way in. */
#include <stdlib.h>
#include <string.h>
#include "utf/collate.h"
#include "locale_names.h"
#include "../../common/s32utf.h"

typedef struct { const char *name; const utf_collator *coll; } cob_locale;
static cob_locale locs[COB_NLOCALES];
static int locs_ready;

static void locs_init(void)
{
    if (locs_ready) return;
    for (int i = 0; i < COB_NLOCALES; i++) {
        locs[i].name = cob_locale_names[i];
        locs[i].coll = i == 0 ? utf_collator_root() : utf_collator_find(cob_locale_names[i]);
        if (!locs[i].coll) locs[i].coll = utf_collator_root();   /* cannot happen: the names are libutf's */
    }
    locs_ready = 1;
}

/* The external name of a locale, resolved to an index, or -1.  A POSIX
 * spelling ("sv_SE.UTF-8", "de_AT@euro") or a BCP 47 one ("sv-SE",
 * "sr-Latn"), case-insensitively; a codeset or modifier is dropped; a name
 * the table lacks falls back a subtag at a time ("de_CH" -> "de"); "C",
 * "POSIX", "root", "und" and the empty name are the POSIX locale. */
int cob_loc_find(const char *name, int n)
{
    char w[64];
    int k = 0;
    locs_init();
    if (n < 0) n = (int)strlen(name);
    for (int i = 0; i < n && k < (int)sizeof w - 1; i++) {
        char c = name[i];
        if (c == '.' || c == '@') break;
        if (c == '-') c = '_';
        if (c == ' ') { if (k == 0) continue; break; }
        w[k++] = c;
    }
    w[k] = 0;
    if (k == 0 || !strcasecmp(w, "C") || !strcasecmp(w, "POSIX") || !strcasecmp(w, "root") || !strcasecmp(w, "und")) return 0;
    for (;;) {
        for (int i = 1; i < COB_NLOCALES; i++) if (!strcasecmp(w, locs[i].name)) return i;
        char *u = strrchr(w, '_');
        if (!u) return -1;
        *u = 0;
    }
}

const char *cob_loc_name(int loc) { locs_init(); return loc >= 0 && loc < COB_NLOCALES ? locs[loc].name : "?"; }

/* --- the current locale: one index per category (8.2, 14.6.6) --- */

static int cur[COB_LC_N], userdef[COB_LC_N];
static int cur_ready;
static int env_missing;   /* the environment named a locale the table lacks: EC-LOCALE-MISSING when it is used */
static const char *const cat_env[COB_LC_N] = { "LC_COLLATE", "LC_CTYPE", "LC_MESSAGES", "LC_MONETARY", "LC_NUMERIC", "LC_TIME" };

static int env_locale(const char *var, int *missing)
{
    const char *v = getenv(var);
    if (!v || !*v) return -1;
    int i = cob_loc_find(v, -1);
    if (i < 0) { *missing = 1; return 0; }
    return i;
}

/* The user default locale, from the environment the POSIX way: LC_ALL,
 * else the category's own variable, else LANG, else POSIX.  Read once at
 * the first use; cob_loc_init_env reads it again (SET ... TO USER-DEFAULT
 * after a non-COBOL module changed it, 8.2 -- and the tests). */
void cob_loc_init_env(void)
{
    int missing = 0;
    int all = env_locale("LC_ALL", &missing);
    int lang = all >= 0 ? -1 : env_locale("LANG", &missing);
    for (int c = 0; c < COB_LC_N; c++) {
        int v = all;
        if (v < 0) v = env_locale(cat_env[c], &missing);
        if (v < 0) v = lang;
        if (v < 0) v = 0;
        userdef[c] = v;
    }
    env_missing = missing;
    cur_ready = 1;
}

static void cur_init(void)
{
    if (cur_ready) return;
    cob_loc_init_env();
    for (int c = 0; c < COB_LC_N; c++) cur[c] = userdef[c];
}

int cob_loc_current(int cat) { cur_init(); return cat >= 0 && cat < COB_LC_N ? cur[cat] : cur[0]; }
int cob_loc_user_default(int cat) { cur_init(); return cat >= 0 && cat < COB_LC_N ? userdef[cat] : userdef[0]; }
int cob_loc_env_missing(void) { cur_init(); int r = env_missing; env_missing = 0; return r; }

/* SET LOCALE category TO loc: cat -1 is LC_ALL; loc the index, or -2 for
 * USER-DEFAULT, -3 for SYSTEM-DEFAULT (POSIX) */
void cob_loc_set(int cat, int loc)
{
    cur_init();
    for (int c = 0; c < COB_LC_N; c++) {
        if (cat >= 0 && c != cat) continue;
        cur[c] = loc == -2 ? userdef[c] : loc == -3 ? 0 : loc;
    }
}
/* SET LOCALE USER-DEFAULT TO loc (14.9.39.4 rule 22) */
void cob_loc_set_user_default(int loc)
{
    cur_init();
    for (int c = 0; c < COB_LC_N; c++) userdef[c] = loc == -3 ? 0 : loc;
}

/* --- saved locales: SET format 12 saves, format 11 restores (rules 21, 26-27) --- */

#define SAVED_MAGIC 0x4c4f4341u   /* 'LOCA' */
typedef struct { unsigned magic; int cat[COB_LC_N]; } cob_saved_locale;

void *cob_loc_save(int user_default)
{
    cur_init();
    cob_saved_locale *s = malloc(sizeof *s);
    if (!s) return NULL;
    s->magic = SAVED_MAGIC;
    for (int c = 0; c < COB_LC_N; c++) s->cat[c] = user_default ? userdef[c] : cur[c];
    return s;
}
/* 1 when p is not a saved locale: EC-LOCALE-INVALID-PTR */
int cob_loc_restore(int cat, const void *p)
{
    const cob_saved_locale *s = p;
    cur_init();
    if (!s || ((unsigned long)s & 3) || s->magic != SAVED_MAGIC) return 1;
    for (int c = 0; c < COB_LC_N; c++) if (cat < 0 || c == cat) cur[c] = s->cat[c];
    return 0;
}

/* --- comparison (8.8.4.2.11, 15.51, 15.85) --- */

/* An operand as UTF-8 with its trailing spaces gone -- all spaces to one
 * space, the empty operand left empty.  National (UTF-16BE) is decoded
 * through the model s32utf.h keeps; a lone surrogate is U+FFFD. */
static int to_utf8(const unsigned char *p, int n, int national, unsigned char *out, int max)
{
    int k = 0;
    if (national) {
        int units = n / 2;
        while (units > 0 && s32u_u16_at(p, (size_t)units - 1) == ' ') units--;
        if (units == 0 && n > 0) units = 1;        /* the one space; p[0..1] is a space */
        for (size_t i = 0; i < (size_t)units; ) {
            uint32_t cp;
            size_t used = s32u_u16_get(p, (size_t)units, i, &cp);
            if (used == 0) { cp = 0xFFFD; used = 1; }
            i += used;
            if (k + 4 > max) break;
            k += s32u_encode(cp, out + k);
        }
        return k;
    }
    int n0 = n;
    while (n > 0 && p[n - 1] == ' ') n--;
    if (n == 0) { if (n0 > 0 && max > 0) { out[0] = ' '; return 1; } return 0; }   /* all spaces: one; empty: empty */
    if (n > max) n = max;
    memcpy(out, p, (size_t)n);
    return n;
}

/* Where a sort key's level ends (libutf's layout: 16-bit primaries, a
 * 0x0000, 16-bit secondaries, 0x0000, tertiary bytes, then a 0x00 and the
 * NFC tiebreak).  The 16-bit levels are scanned in units: a weight's low
 * byte can be 0x00, so a byte scan would stop early. */
static int key_cut(const unsigned char *key, int n, int level)
{
    int pos = 0;
    for (int lvl = 1; lvl <= 2; lvl++) {
        while (pos + 1 < n && (key[pos] | key[pos + 1])) pos += 2;
        if (level == lvl) return pos;
        pos += 2;                                   /* past the separator */
    }
    if (level == 3) { while (pos < n && key[pos]) pos++; return pos; }
    return n;
}

/* cob_loc_compare: -1, 0, 1 for a before, equal to, after b.  nat_a / nat_b
 * say which operands are national.  loc is a locale index, or -1 for the
 * current LC_COLLATE.  level 0 compares at every level (LOCALE-COMPARE,
 * and STANDARD-COMPARE without argument-4); 1..4 at that many levels
 * (STANDARD-COMPARE's argument-4; 4 is everything, the code-point
 * tiebreak included).  A key too long for the buffers falls back to the
 * full comparison. */
#define LOC_BUF 4096
static unsigned char ua_s[LOC_BUF], ub_s[LOC_BUF], ka_s[LOC_BUF * 4], kb_s[LOC_BUF * 4];

int cob_loc_compare(const unsigned char *a, int na, int nat_a, const unsigned char *b, int nb, int nat_b, int loc, int level)
{
    locs_init();
    if (loc < 0 || loc >= COB_NLOCALES) loc = cob_loc_current(COB_LC_COLLATE);
    const utf_collator *c = locs[loc].coll;
    unsigned char *ua = ua_s, *ub = ub_s;
    int big_a = na * 2 > LOC_BUF, big_b = nb * 2 > LOC_BUF;
    if (big_a) ua = malloc((size_t)na * 2 + 4);
    if (big_b) ub = malloc((size_t)nb * 2 + 4);
    if (!ua || !ub) { if (big_a && ua) free(ua); if (big_b && ub) free(ub); return memcmp(a, b, (size_t)(na < nb ? na : nb)) < 0 ? -1 : 1; }
    int la = to_utf8(a, na, nat_a, ua, big_a ? na * 2 + 4 : LOC_BUF);
    int lb = to_utf8(b, nb, nat_b, ub, big_b ? nb * 2 + 4 : LOC_BUF);
    int r;
    if (level >= 1 && level <= 3 && !big_a && !big_b) {
        size_t ma = utf_collate_sortkey_l(ua, (size_t)la, ka_s, sizeof ka_s, c);
        size_t mb = utf_collate_sortkey_l(ub, (size_t)lb, kb_s, sizeof kb_s, c);
        if (ma < sizeof ka_s && mb < sizeof kb_s) {
            int ca = key_cut(ka_s, (int)ma, level), cb = key_cut(kb_s, (int)mb, level);
            int m = ca < cb ? ca : cb;
            r = memcmp(ka_s, kb_s, (size_t)m);
            if (r == 0) r = ca - cb;
            goto done;
        }
    }
    r = utf_collate_cmp_l(ua, (size_t)la, ub, (size_t)lb, c);
done:
    if (big_a) free(ua);
    if (big_b) free(ub);
    return r < 0 ? -1 : r > 0 ? 1 : 0;
}

/* the collator's own name, for the tests and the SOURCE check */
const char *cob_loc_collator_name(int loc) { locs_init(); return loc >= 0 && loc < COB_NLOCALES ? utf_collator_name(locs[loc].coll) : NULL; }
