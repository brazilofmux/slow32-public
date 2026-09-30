/* esql.c -- the EXEC SQL runtime (docs/esql.md).
 *
 * The compiler turns each embedded SQL statement into a descriptor (its
 * text with host references as ?, a slot for the prepared statement, its
 * kind) and, before the call that runs it, one cob_sql_in / cob_sql_out
 * call per host reference.  This file keeps the connection, prepares each
 * statement once, binds and fetches through the COBOL descriptors, and
 * answers with SQLCODE and SQLSTATE.
 *
 * The database is SQLite, taken as it is.  Each schema (authorization id)
 * is a database file in COB_SQL_DIR (default "."), named <SCHEMA>.db and
 * listed in the catalog file "schemas" there.  The connected user's file
 * is `main` and the others are attached under their names, so HU.STAFF
 * names HU's table from anywhere; the user's own qualifier becomes
 * `main.`.  The user is set by CALL "AUTHID" USING name (the NIST
 * suite's login routine), or COB_SQL_USER, else PUBLIC.
 *
 * A separate object from libcob, so a program without SQL does not pull
 * SQLite in; compile.sh links it, with libsqlite3.s32a, when it sees SQL. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cobrt.h"
#include "sqlite3.h"

long long cob_get_num(const void *p, const cob_desc *d);
void cob_put_num(void *p, const cob_desc *d, long long v, int vscale);
void cob_move(const void *src, const cob_desc *sd, void *dst, const cob_desc *dd);

/* the compiler's descriptors (s32-cobc.c emit_sql_data) */
typedef struct cob_sql_cursor {
    const char *text;           /* the query; SELECT rowid, ... when positioned */
    sqlite3_stmt *st;
    int positioned;             /* its first column is the rowid */
    int open;                   /* 1 open, 2 open and past its last row */
    unsigned rowid_lo, rowid_hi;
    const char *name;
} cob_sql_cursor;
typedef struct {
    const char *text;
    sqlite3_stmt *st;
    int kind;                   /* 0 run, 1 SELECT INTO, 2 no-op (GRANT/REVOKE), 3 positioned */
    cob_sql_cursor *cur;
} cob_sql_stmt;
enum { K_EXEC = 0, K_SELECT_INTO = 1, K_NOOP = 2, K_POSITIONED = 3 };

typedef struct { void *p; const cob_desc *d; void *ip; const cob_desc *id; } host;
static host g_in[128], g_out[128];
static int g_nin, g_nout;

static sqlite3 *g_db;
static char g_user[64];
static int g_trace = -1;
static int g_sqlcode;
static char g_sqlstate[6] = "00000";

/* every statement and cursor prepared on this connection, to finalize
 * when it closes */
static sqlite3_stmt **g_prepared_slot[4096]; static int g_nprep;
static cob_sql_cursor *g_cursors[512]; static int g_ncur;

static void set_status(int code, const char *state)
{
    g_sqlcode = code;
    memcpy(g_sqlstate, state, 5); g_sqlstate[5] = 0;
}

static void trace_error(const char *what)
{
    if (g_trace < 0) { const char *t = getenv("COB_SQL_TRACE"); g_trace = t && *t && *t != '0'; }
    if (g_trace) fprintf(stderr, "esql: %s: SQLCODE %d SQLSTATE %s: %s\n", what, g_sqlcode, g_sqlstate, g_db ? sqlite3_errmsg(g_db) : "no connection");
}

/* the backend's error as SQLCODE and SQLSTATE */
static void set_error(int rc, const char *what)
{
    int ext = g_db ? sqlite3_extended_errcode(g_db) : rc;
    switch (ext) {
    case SQLITE_CONSTRAINT_UNIQUE: case SQLITE_CONSTRAINT_PRIMARYKEY: set_status(-803, "23000"); break;
    case SQLITE_CONSTRAINT_NOTNULL: set_status(-407, "23000"); break;
    case SQLITE_CONSTRAINT_CHECK: set_status(-545, "23000"); break;
    case SQLITE_CONSTRAINT_FOREIGNKEY: set_status(-530, "23000"); break;
    default:
        if ((ext & 0xff) == SQLITE_CONSTRAINT) set_status(-803, "23000");
        else if (rc == SQLITE_ERROR) set_status(-204, "42000");        /* syntax, no such table or column */
        else if (rc == SQLITE_MISMATCH || rc == SQLITE_RANGE) set_status(-302, "22000");
        else set_status(-1, "58000");
    }
    trace_error(what);
}

/* ---- the connection ---------------------------------------------------- */

static const char *sql_dir(void) { const char *d = getenv("COB_SQL_DIR"); return d && *d ? d : "."; }

static void upcase_trim(char *out, const char *in, int n, int cap)
{
    int k = 0;
    for (int i = 0; i < n && in[i] && k < cap - 1; i++) out[k++] = (char)toupper((unsigned char)in[i]);
    while (k && out[k - 1] == ' ') k--;
    out[k] = 0;
}

static void disconnect(void)
{
    if (!g_db) return;
    for (int i = 0; i < g_nprep; i++) { sqlite3_finalize(*g_prepared_slot[i]); *g_prepared_slot[i] = NULL; }
    g_nprep = 0;
    for (int i = 0; i < g_ncur; i++) g_cursors[i]->open = 0;
    g_ncur = 0;
    sqlite3_exec(g_db, "COMMIT", 0, 0, 0);
    sqlite3_close(g_db);
    g_db = NULL;
}

/* the schema's name in the catalog, added if new */
static void catalog_add(const char *schema)
{
    char path[512], line[128];
    snprintf(path, sizeof path, "%s/schemas", sql_dir());
    FILE *f = fopen(path, "r");
    if (f) {
        while (fgets(line, sizeof line, f)) {
            line[strcspn(line, "\r\n")] = 0;
            if (!strcmp(line, schema)) { fclose(f); return; }
        }
        fclose(f);
    }
    f = fopen(path, "a");
    if (f) { fprintf(f, "%s\n", schema); fclose(f); }
}

static int sql_connect(void)
{
    if (g_db) return 1;
    if (!g_user[0]) {
        const char *u = getenv("COB_SQL_USER");
        upcase_trim(g_user, u && *u ? u : "PUBLIC", 63, sizeof g_user);
    }
    char path[512];
    snprintf(path, sizeof path, "%s/%s.db", sql_dir(), g_user);
    if (sqlite3_open_v2(path, &g_db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL) != SQLITE_OK) {
        set_status(-1, "08001"); trace_error("connect");
        sqlite3_close(g_db); g_db = NULL;
        return 0;
    }
    catalog_add(g_user);
    char cat[512], line[128];
    snprintf(cat, sizeof cat, "%s/schemas", sql_dir());
    FILE *f = fopen(cat, "r");
    if (f) {
        while (fgets(line, sizeof line, f)) {
            line[strcspn(line, "\r\n")] = 0;
            if (!line[0] || !strcmp(line, g_user)) continue;
            char *q = sqlite3_mprintf("ATTACH %Q AS \"%w\"", sqlite3_mprintf("%s/%s.db", sql_dir(), line), line);
            sqlite3_exec(g_db, q, 0, 0, 0);
            sqlite3_free(q);
        }
        fclose(f);
    }
    return 1;
}

/* CALL "AUTHID" USING name: the NIST suite's login -- the user whose
 * schema unqualified names mean */
void authid(char *uid)
{
    char u[64];
    upcase_trim(u, uid, 18, sizeof u);
    if (!u[0]) return;
    if (g_db && strcmp(u, g_user)) disconnect();
    snprintf(g_user, sizeof g_user, "%s", u);
}

/* ---- host variables ---------------------------------------------------- */

void cob_sql_in(void *p, const cob_desc *d, void *ip, const cob_desc *id)
{
    if (g_nin == 128) { fprintf(stderr, "esql: more than 128 input host variables\n"); exit(1); }
    g_in[g_nin].p = p; g_in[g_nin].d = d; g_in[g_nin].ip = ip; g_in[g_nin].id = id; g_nin++;
}

void cob_sql_out(void *p, const cob_desc *d, void *ip, const cob_desc *id)
{
    if (g_nout == 128) { fprintf(stderr, "esql: more than 128 output host variables\n"); exit(1); }
    g_out[g_nout].p = p; g_out[g_nout].d = d; g_out[g_nout].ip = ip; g_out[g_nout].id = id; g_nout++;
}

static void clear_hosts(void) { g_nin = g_nout = 0; }

static const long long p10[19] = { 1LL, 10LL, 100LL, 1000LL, 10000LL, 100000LL, 1000000LL, 10000000LL, 100000000LL,
    1000000000LL, 10000000000LL, 100000000000LL, 1000000000000LL, 10000000000000LL, 100000000000000LL,
    1000000000000000LL, 10000000000000000LL, 100000000000000000LL, 1000000000000000000LL };

/* one input: NULL by a negative indicator; a numeric item as an integer
 * or its exact decimal text; anything else as text, trailing spaces
 * trimmed (SQL-92 compares CHAR blank-padded, SQLite exactly) */
static int bind_in(sqlite3_stmt *st, int i, const host *h)
{
    if (h->ip && cob_get_num(h->ip, h->id) < 0) return sqlite3_bind_null(st, i);
    const cob_desc *d = h->d;
    if (d->cat == COB_NUM) {
        long long v = cob_get_num(h->p, d);
        int sc = d->scale;
        if (sc <= 0) {
            for (int k = 0; k < -sc && k < 18; k++) v *= 10;
            return sqlite3_bind_int64(st, i, v);
        }
        char buf[48];
        unsigned long long m = v < 0 ? 0 - (unsigned long long)v : (unsigned long long)v;
        unsigned long long ip = m / (unsigned long long)p10[sc > 18 ? 18 : sc], fp = m % (unsigned long long)p10[sc > 18 ? 18 : sc];
        snprintf(buf, sizeof buf, "%s%llu.%0*llu", v < 0 ? "-" : "", ip, sc, fp);
        return sqlite3_bind_text(st, i, buf, -1, SQLITE_TRANSIENT);
    }
    int n = (int)d->size;
    const char *s = h->p;
    while (n && s[n - 1] == ' ') n--;
    return sqlite3_bind_text(st, i, s, n, SQLITE_TRANSIENT);
}

/* the decimal text of a value as a scaled integer: *v at scale *sc; 0 when
 * it is not a number, -1 when it does not fit 18 digits */
static int parse_decimal(const char *t, long long *v, int *sc)
{
    while (*t == ' ') t++;
    int neg = 0;
    if (*t == '+' || *t == '-') neg = *t++ == '-';
    unsigned long long m = 0; int digits = 0, scale = 0, pt = 0, any = 0;
    for (; *t; t++) {
        if (*t >= '0' && *t <= '9') {
            any = 1;
            if (!digits && *t == '0' && !pt) continue;
            if (digits < 18) { m = m * 10 + (unsigned)(*t - '0'); digits++; if (pt) scale++; }
            else if (!pt) return -1;                 /* an integer part past 18 digits */
        } else if (*t == '.' && !pt) pt = 1;
        else break;
    }
    if (!any) return 0;
    if (*t == 'e' || *t == 'E') {
        int e = atoi(t + 1);
        scale -= e;
        while (scale < 0) { if (m > 922337203685477580ULL) return -1; m *= 10; scale++; }
        while (scale > 18) { m /= 10; scale--; }
    }
    *v = neg ? -(long long)m : (long long)m;
    *sc = scale;
    return 1;
}

/* one output, column c: NULL to the indicator (or an error), a number by
 * its exact text, anything else by the MOVE rules */
static int store_out(sqlite3_stmt *st, int c, const host *h)
{
    int ty = sqlite3_column_type(st, c);
    if (ty == SQLITE_NULL) {
        if (!h->ip) { set_status(-305, "22002"); trace_error("fetch: NULL with no indicator"); return 0; }
        cob_put_num(h->ip, h->id, -1, 0);
        return 1;
    }
    if (h->ip) cob_put_num(h->ip, h->id, 0, 0);
    const cob_desc *d = h->d;
    if (d->cat == COB_NUM) {
        long long v; int sc;
        if (ty == SQLITE_INTEGER) { v = sqlite3_column_int64(st, c); sc = 0; }
        else {
            const char *t = (const char *)sqlite3_column_text(st, c);
            int r = parse_decimal(t ? t : "", &v, &sc);
            if (r <= 0) { set_status(r < 0 ? -304 : -302, r < 0 ? "22003" : "22018"); trace_error("fetch: not a number"); return 0; }
        }
        cob_put_num(h->p, d, v, sc);
        return 1;
    }
    const char *t = (const char *)sqlite3_column_text(st, c);
    int n = sqlite3_column_bytes(st, c);
    cob_desc sd; memset(&sd, 0, sizeof sd);
    sd.cat = COB_ALNUM; sd.size = (unsigned)n;
    if (n) cob_move(t, &sd, h->p, d);
    else memset(h->p, ' ', d->size);
    if (h->ip && n > (int)d->size) cob_put_num(h->ip, h->id, n, 0);   /* truncated: the indicator says how long it was */
    return 1;
}

/* ---- statements -------------------------------------------------------- */

/* the statement as SQLite takes it, outside quotes: the user's own schema
 * qualifier as main., and the user special values (USER, CURRENT_USER,
 * SESSION_USER, SYSTEM_USER -- reserved words, so never a column) as the
 * user's name, a literal: SQLite has no users */
static int wordat(const char *t, size_t i, const char *w)
{
    size_t n = strlen(w);
    return !strncasecmp(t + i, w, n) && !(isalnum((unsigned char)t[i + n]) || t[i + n] == '_' || t[i + n] == '.' || t[i + n] == '(') &&
           (i == 0 || !(isalnum((unsigned char)t[i - 1]) || t[i - 1] == '_' || t[i - 1] == '.'));
}
static char *own_schema(const char *text)
{
    static const char *uservals[] = { "current_user", "session_user", "system_user", "user", NULL };
    size_t ul = strlen(g_user), n = strlen(text);
    int nuv = 0; for (size_t i = 0; i < n; i++) if (text[i] == 'u' || text[i] == 'U') nuv++;
    char *out = malloc(n * 2 + 8 + (size_t)nuv * (ul + 2)); size_t o = 0;
    char quote = 0;
    for (size_t i = 0; i < n; ) {
        char c = text[i];
        if (quote) { if (c == quote) quote = 0; out[o++] = text[i++]; continue; }
        if (c == '\'') { quote = c; out[o++] = text[i++]; continue; }
        int uv = -1;
        for (int k = 0; uservals[k] && uv < 0; k++) if (wordat(text, i, uservals[k])) uv = k;
        if (uv >= 0) {
            out[o++] = '\''; memcpy(out + o, g_user, ul); o += ul; out[o++] = '\'';
            i += strlen(uservals[uv]);
            continue;
        }
        if (ul && !strncasecmp(text + i, g_user, ul) && text[i + ul] == '.' &&
            (i == 0 || !(isalnum((unsigned char)text[i - 1]) || text[i - 1] == '_' || text[i - 1] == '.'))) {
            memcpy(out + o, "main.", 5); o += 5; i += ul + 1;
            continue;
        }
        out[o++] = text[i++];
    }
    out[o] = 0;
    return out;
}

static int prepare(sqlite3_stmt **slot, const char *text)
{
    if (*slot) return 1;
    char *t = own_schema(text);
    int rc = sqlite3_prepare_v2(g_db, t, -1, slot, NULL);
    free(t);
    if (rc != SQLITE_OK) { set_error(rc, text); *slot = NULL; return 0; }
    if (g_nprep < 4096) g_prepared_slot[g_nprep++] = slot;
    return 1;
}

/* SQL-92: a transaction begins with the first statement after the last
 * one ended */
static void begin_if_needed(void)
{
    if (sqlite3_get_autocommit(g_db)) sqlite3_exec(g_db, "BEGIN", 0, 0, 0);
}

static int first_word(const char *t, const char *w)
{
    while (*t == ' ') t++;
    size_t n = strlen(w);
    return !strncasecmp(t, w, n) && !isalnum((unsigned char)t[n]);
}

/* COMMIT or ROLLBACK [WORK]: ends the transaction, closing the cursors */
static int end_transaction(int commit)
{
    for (int i = 0; i < g_ncur; i++) if (g_cursors[i]->open) { sqlite3_reset(g_cursors[i]->st); g_cursors[i]->open = 0; }
    g_ncur = 0;
    if (!sqlite3_get_autocommit(g_db)) {
        int rc = sqlite3_exec(g_db, commit ? "COMMIT" : "ROLLBACK", 0, 0, 0);
        if (rc != SQLITE_OK) { set_error(rc, commit ? "COMMIT" : "ROLLBACK"); return g_sqlcode; }
    }
    return 0;
}

int cob_sql_exec(cob_sql_stmt *s)
{
    set_status(0, "00000");
    if (s->kind == K_NOOP) { clear_hosts(); return 0; }       /* GRANT, REVOKE: SQLite has no privileges */
    if (!sql_connect()) { clear_hosts(); return g_sqlcode; }
    if (first_word(s->text, "commit") || first_word(s->text, "rollback")) {
        clear_hosts();
        return end_transaction(first_word(s->text, "commit"));
    }
    begin_if_needed();
    if (!prepare(&s->st, s->text)) { clear_hosts(); return g_sqlcode; }
    sqlite3_stmt *st = s->st;
    int i = 0;
    for (; i < g_nin; i++) bind_in(st, i + 1, &g_in[i]);
    if (s->kind == K_POSITIONED) {
        cob_sql_cursor *c = s->cur;
        if (!c->open) { set_status(-508, "24000"); trace_error("positioned: cursor not open"); goto done; }
        sqlite3_bind_int64(st, i + 1, (long long)(((unsigned long long)c->rowid_hi << 32) | c->rowid_lo));
    }
    int rc = sqlite3_step(st);
    if (s->kind == K_SELECT_INTO) {
        if (rc == SQLITE_ROW) {
            for (int k = 0; k < g_nout && k < sqlite3_column_count(st); k++) if (!store_out(st, k, &g_out[k])) goto done;
            if (sqlite3_step(st) == SQLITE_ROW) { set_status(-811, "21000"); trace_error("SELECT INTO: more than one row"); }
        } else if (rc == SQLITE_DONE) set_status(100, "02000");
        else set_error(rc, s->text);
    } else {
        while (rc == SQLITE_ROW) rc = sqlite3_step(st);
        if (rc != SQLITE_DONE) set_error(rc, s->text);
        else if ((first_word(s->text, "update") || first_word(s->text, "delete") || first_word(s->text, "insert")) &&
                 sqlite3_changes(g_db) == 0) set_status(100, "02000");      /* no row: no data (SQL-92) */
    }
done:
    sqlite3_reset(st);
    sqlite3_clear_bindings(st);
    clear_hosts();
    return g_sqlcode;
}

int cob_sql_open(cob_sql_cursor *c)
{
    set_status(0, "00000");
    if (!sql_connect()) { clear_hosts(); return g_sqlcode; }
    if (c->open) { set_status(-502, "24000"); trace_error(c->name); clear_hosts(); return g_sqlcode; }
    begin_if_needed();
    if (!prepare(&c->st, c->text)) { clear_hosts(); return g_sqlcode; }
    sqlite3_reset(c->st); sqlite3_clear_bindings(c->st);
    for (int i = 0; i < g_nin; i++) bind_in(c->st, i + 1, &g_in[i]);
    clear_hosts();
    c->open = 1;
    if (g_ncur < 512) g_cursors[g_ncur++] = c;
    return 0;
}

int cob_sql_fetch(cob_sql_cursor *c)
{
    set_status(0, "00000");
    if (!c->open || !c->st) { set_status(-501, "24000"); trace_error(c->name); clear_hosts(); return g_sqlcode; }
    /* past the last row, no data again: a step after SQLITE_DONE would
     * start the query over */
    if (c->open == 2) { set_status(100, "02000"); clear_hosts(); return g_sqlcode; }
    int rc = sqlite3_step(c->st);
    if (rc == SQLITE_ROW) {
        int base = 0;
        if (c->positioned) {
            unsigned long long r = (unsigned long long)sqlite3_column_int64(c->st, 0);
            c->rowid_lo = (unsigned)r; c->rowid_hi = (unsigned)(r >> 32);
            base = 1;
        }
        for (int k = 0; k < g_nout && base + k < sqlite3_column_count(c->st); k++)
            if (!store_out(c->st, base + k, &g_out[k])) break;
    } else if (rc == SQLITE_DONE) { set_status(100, "02000"); c->open = 2; }
    else set_error(rc, c->name);
    clear_hosts();
    return g_sqlcode;
}

int cob_sql_close(cob_sql_cursor *c)
{
    set_status(0, "00000");
    clear_hosts();
    if (!c->open) { set_status(-501, "24000"); trace_error(c->name); return g_sqlcode; }
    sqlite3_reset(c->st);
    c->open = 0;
    return 0;
}

/* ---- the program's status items ---------------------------------------- */

void cob_sql_put_sqlcode(void *p, const cob_desc *d)
{
    if (d->cat == COB_NUM) cob_put_num(p, d, g_sqlcode, 0);
}

void cob_sql_put_sqlstate(void *p, const cob_desc *d)
{
    cob_desc sd; memset(&sd, 0, sizeof sd);
    sd.cat = COB_ALNUM; sd.size = 5;
    cob_move(g_sqlstate, &sd, p, d);
}
