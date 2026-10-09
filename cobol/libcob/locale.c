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

/* the external name resolved to an index, or -1 (locale_names.h has the rule) */
int cob_loc_find(const char *name, int n) { locs_init(); return cob_locale_index(name, n); }

const char *cob_loc_name(int loc) { locs_init(); return loc >= 0 && loc < COB_NLOCALES ? locs[loc].name : "?"; }

/* --- the current locale: one index per category (8.2, 14.6.6) --- */

static int cur[COB_LC_N], userdef[COB_LC_N];
static int cur_ready;
/* the environment named a locale the table lacks: the user default is
 * POSIX in its place, and a category holding that default is "missing"
 * (8.2: EC-LOCALE-MISSING when an operation needs it) until a SET gives
 * it a locale that exists */
static int env_missing, ud_missing[COB_LC_N], cur_missing[COB_LC_N];
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
    int m_all = 0, m_lang = 0, m_any;
    int all = env_locale("LC_ALL", &m_all);
    int lang = all >= 0 ? -1 : env_locale("LANG", &m_lang);
    m_any = m_all | m_lang;
    for (int c = 0; c < COB_LC_N; c++) {
        int v = all, m = m_all, m_cat = 0;
        if (v < 0) { v = env_locale(cat_env[c], &m_cat); m = m_cat; m_any |= m_cat; }
        if (v < 0) { v = lang; m = m_lang; }
        if (v < 0) { v = 0; m = 0; }
        userdef[c] = v; ud_missing[c] = m;
    }
    env_missing = m_any;
    cur_ready = 1;
}

static void cur_init(void)
{
    if (cur_ready) return;
    cob_loc_init_env();
    for (int c = 0; c < COB_LC_N; c++) { cur[c] = userdef[c]; cur_missing[c] = ud_missing[c]; }
}

int cob_loc_current(int cat) { cur_init(); return cat >= 0 && cat < COB_LC_N ? cur[cat] : cur[0]; }
int cob_loc_user_default(int cat) { cur_init(); return cat >= 0 && cat < COB_LC_N ? userdef[cat] : userdef[0]; }
/* 1 when the category's current locale stands in for one the environment
 * named and the table lacks */
int cob_loc_missing(int cat) { cur_init(); return cat >= 0 && cat < COB_LC_N ? cur_missing[cat] : cur_missing[0]; }
int cob_loc_env_missing(void) { cur_init(); return env_missing; }

/* SET LOCALE category TO loc: cat -1 is LC_ALL; loc the index, or -2 for
 * USER-DEFAULT, -3 for SYSTEM-DEFAULT (POSIX).  Returns 1 when a category
 * set from the user default got the stand-in for a missing locale:
 * EC-LOCALE-MISSING (14.9.39.4 rule 24) */
int cob_loc_set(int cat, int loc)
{
    int miss = 0;
    cur_init();
    for (int c = 0; c < COB_LC_N; c++) {
        if (cat >= 0 && c != cat) continue;
        cur[c] = loc == -2 ? userdef[c] : loc == -3 ? 0 : loc;
        cur_missing[c] = loc == -2 ? ud_missing[c] : 0;
        miss |= cur_missing[c];
    }
    return miss;
}
/* SET LOCALE USER-DEFAULT TO loc (14.9.39.4 rule 22) */
void cob_loc_set_user_default(int loc)
{
    cur_init();
    for (int c = 0; c < COB_LC_N; c++) { userdef[c] = loc == -3 ? 0 : loc; ud_missing[c] = 0; }
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
    for (int c = 0; c < COB_LC_N; c++) if (cat < 0 || c == cat) { cur[c] = s->cat[c]; cur_missing[c] = 0; }
    return 0;
}
/* SET LOCALE USER-DEFAULT TO identifier: the user default from a saved
 * locale (rule 22); 1 when p is not one */
int cob_loc_user_default_from(const void *p)
{
    const cob_saved_locale *s = p;
    cur_init();
    if (!s || ((unsigned long)s & 3) || s->magic != SAVED_MAGIC) return 1;
    for (int c = 0; c < COB_LC_N; c++) { userdef[c] = s->cat[c]; ud_missing[c] = 0; }
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

/* --- the intrinsic functions LOCALE-COMPARE and STANDARD-COMPARE (15.51,
 * 15.85): argument-1 staged, then the call with argument-2.  The result
 * is one character, '<' '=' or '>'; the compiler reads a note after the
 * call for the exception conditions (as it does cob_fn_argbad). --- */

static const unsigned char *fn_a; static int fn_na, fn_nat_a, fn_level, fn_bad;
void cob_loc_fn_arg(const unsigned char *p, int n, int nat) { fn_a = p; fn_na = n; fn_nat_a = nat; }
void cob_loc_fn_level(int level) { fn_level = level; }
/* loc: the locale index, or -1 for the current LC_COLLATE (LOCALE-COMPARE),
 * 0 for the ordering table (STANDARD-COMPARE: the root collation is
 * 'ISO_14651_2020_TABLE1' here); standard: 1 for STANDARD-COMPARE, whose
 * level fn_level holds (0: the table's highest) */
char *cob_loc_fn_compare(const unsigned char *b, int nb, int nat_b, int loc, int standard)
{
    static char res[2];
    int level = 0;
    if (standard) {
        level = fn_level; fn_level = 0;
        if (level < 0 || level > 4) { fn_bad = 2; level = 0; }   /* EC-ORDER-NOT-SUPPORTED: not a level of the table */
    } else if (loc < 0 && cob_loc_missing(COB_LC_COLLATE)) fn_bad = 1;   /* EC-LOCALE-MISSING: the current locale stands in for a missing one */
    int r = cob_loc_compare(fn_a, fn_na, fn_nat_a, b, nb, nat_b, loc, level);
    res[0] = r < 0 ? '<' : r > 0 ? '>' : '=';
    res[1] = 0;
    return res;
}
/* the note: 0 fine, 1 EC-LOCALE-MISSING, 2 EC-ORDER-NOT-SUPPORTED; cleared */
int cob_loc_fn_bad(void) { int r = fn_bad; fn_bad = 0; return r; }

/* the collator's own name, for the tests and the SOURCE check */
const char *cob_loc_collator_name(int loc) { locs_init(); return loc >= 0 && loc < COB_NLOCALES ? utf_collator_name(locs[loc].coll) : NULL; }

/* --- LC_TIME: LOCALE-DATE, LOCALE-TIME, LOCALE-TIME-FROM-SECONDS (15.52-15.54;
 * docs/plans/locale.md step 2).  The record's patterns are CLDR's (locale_data.h,
 * generated from CLDR 46 by gen_locale_data.py): the medium date and time
 * formats stand for d_fmt and t_fmt, expanded here from the record's names.
 * The result is alphanumeric (UTF-8), of run-time length; an argument outside
 * the rules is EC-ARGUMENT-FUNCTION, noted in libcob (cob_fn_argbad_set) and
 * an empty result. --- */
#include "locale_data.h"

char *cob_fn_var_result(const char *s, int n);
void cob_fn_argbad_set(void);

static int leap_year(int y) { return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0; }
static int month_days(int y, int m) { static const int d[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }; return d[m - 1] + (m == 2 && leap_year(y)); }
/* 0 Sunday .. 6 Saturday (Zeller) */
static int weekday(int y, int m, int d)
{
    if (m < 3) { m += 12; y--; }
    int k = y % 100, j = y / 100;
    int h = (d + 13 * (m + 1) / 5 + k + k / 4 + j / 4 + 5 * j) % 7;   /* 0 Saturday */
    return (h + 6) % 7;
}

typedef struct { int y, mo, d, h, mi, s; int has_date, has_time; } cob_tm;

static void put(char *out, int *k, int max, const char *s)
{
    while (*s && *k < max - 1) out[(*k)++] = *s++;
}
static void put_num(char *out, int *k, int max, int v, int width)
{
    char b[12]; int n = 0;
    if (v < 0) v = 0;
    do { b[n++] = (char)('0' + v % 10); v /= 10; } while (v);
    while (n < width) b[n++] = '0';
    while (n && *k < max - 1) out[(*k)++] = b[--n];
}

/* a CLDR date or time pattern (y M L d E c H k h K m s a b B, quoted text) */
static int expand(const cob_locale_data *ld, const char *pat, const cob_tm *t, char *out, int max)
{
    int k = 0;
    for (const char *p = pat; *p; ) {
        char c = *p;
        if (c == '\'') {
            if (p[1] == '\'') { if (k < max - 1) out[k++] = '\''; p += 2; continue; }
            for (p++; *p && *p != '\''; p++) if (k < max - 1) out[k++] = *p;
            if (*p) p++;
            continue;
        }
        if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))) { if (k < max - 1) out[k++] = c; p++; continue; }
        int n = 0;
        while (*p == c) { n++; p++; }
        switch (c) {
        case 'y': if (n == 2) put_num(out, &k, max, t->y % 100, 2); else put_num(out, &k, max, t->y, n); break;
        case 'u': put_num(out, &k, max, t->y, n); break;
        case 'M': case 'L':
            if (n >= 4) put(out, &k, max, ld->mon_wide[t->mo - 1]);
            else if (n == 3) put(out, &k, max, ld->mon_abbr[t->mo - 1]);
            else put_num(out, &k, max, t->mo, n);
            break;
        case 'd': put_num(out, &k, max, t->d, n); break;
        case 'E': case 'c': {
            int w = weekday(t->y, t->mo, t->d);
            put(out, &k, max, n >= 4 ? ld->day_wide[w] : ld->day_abbr[w]);
            break;
        }
        case 'H': put_num(out, &k, max, t->h, n); break;
        case 'k': put_num(out, &k, max, t->h == 0 ? 24 : t->h, n); break;
        case 'h': put_num(out, &k, max, t->h % 12 == 0 ? 12 : t->h % 12, n); break;
        case 'K': put_num(out, &k, max, t->h % 12, n); break;
        case 'm': put_num(out, &k, max, t->mi, n); break;
        case 's': put_num(out, &k, max, t->s, n); break;
        case 'a': case 'b': case 'B': put(out, &k, max, t->h < 12 ? ld->am : ld->pm); break;
        case 'G': put(out, &k, max, "AD"); break;
        default: while (n-- && k < max - 1) out[k++] = c; break;
        }
    }
    out[k] = 0;
    return k;
}

/* the argument's characters as ASCII digits; -1 when any is not one */
static int digits_of(const unsigned char *p, int n, int nat, int want, int *v)
{
    int units = nat ? n / 2 : n;
    if (units != want) return -1;
    for (int i = 0; i < want; i++) {
        unsigned c = nat ? s32u_u16_at(p, (size_t)i) : p[i];
        if (c < '0' || c > '9') return -1;
        v[i] = (int)(c - '0');
    }
    return 0;
}

static const cob_locale_data *loc_data(int loc)
{
    if (loc < 0 || loc >= COB_NLOCALES) loc = cob_loc_current(COB_LC_TIME);
    return &cob_locale_data_tab[loc];
}

/* LOCALE-DATE (15.52): argument-1 YYYYMMDD as CURRENT-DATE returns it (1601-9999) */
char *cob_loc_fn_date(const unsigned char *p, int n, int nat, int loc)
{
    int v[8]; cob_tm t = { 0 };
    char out[160];
    if (digits_of(p, n, nat, 8, v)) { cob_fn_argbad_set(); return cob_fn_var_result("", 0); }
    t.y = v[0] * 1000 + v[1] * 100 + v[2] * 10 + v[3]; t.mo = v[4] * 10 + v[5]; t.d = v[6] * 10 + v[7];
    if (t.y < 1601 || t.mo < 1 || t.mo > 12 || t.d < 1 || t.d > month_days(t.y, t.mo)) { cob_fn_argbad_set(); return cob_fn_var_result("", 0); }
    t.has_date = 1;
    int k = expand(loc_data(loc), loc_data(loc)->date_fmt, &t, out, (int)sizeof out);
    return cob_fn_var_result(out, k);
}

/* LOCALE-TIME (15.53): argument-1 hhmmss, hours 00-24, seconds 00-99 (rule 3) */
char *cob_loc_fn_time(const unsigned char *p, int n, int nat, int loc)
{
    int v[6]; cob_tm t = { 0 };
    char out[160];
    if (digits_of(p, n, nat, 6, v)) { cob_fn_argbad_set(); return cob_fn_var_result("", 0); }
    t.h = v[0] * 10 + v[1]; t.mi = v[2] * 10 + v[3]; t.s = v[4] * 10 + v[5];
    if (t.h > 24 || t.mi > 59) { cob_fn_argbad_set(); return cob_fn_var_result("", 0); }
    t.y = 2000; t.mo = 1; t.d = 1; t.has_time = 1;
    int k = expand(loc_data(loc), loc_data(loc)->time_fmt, &t, out, (int)sizeof out);
    return cob_fn_var_result(out, k);
}

/* LOCALE-TIME-FROM-SECONDS (15.54): secs the whole seconds past midnight the
 * compiler popped (cob_pop_seconds: -1 when the value was not in standard
 * numeric time form) */
char *cob_loc_fn_time_secs(int secs, int loc)
{
    cob_tm t = { 0 };
    char out[160];
    if (secs < 0 || secs >= 86400) { cob_fn_argbad_set(); return cob_fn_var_result("", 0); }
    t.h = secs / 3600; t.mi = secs / 60 % 60; t.s = secs % 60;
    t.y = 2000; t.mo = 1; t.d = 1; t.has_time = 1;
    int k = expand(loc_data(loc), loc_data(loc)->time_fmt, &t, out, (int)sizeof out);
    return cob_fn_var_result(out, k);
}
