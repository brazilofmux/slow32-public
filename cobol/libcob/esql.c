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
#include <stddef.h>
#include "cobrt.h"
#include "sqlite3.h"
#include "pgwire.h"

/* a prepared statement: SQLite's sqlite3_stmt, or pgwire's pg_stmt
 * (COB_SQL_BACKEND=postgres) -- opaque to everything but the db_ layer */
typedef void dbst;

long long cob_get_num(const void *p, const cob_desc *d);
void cob_put_num(void *p, const cob_desc *d, long long v, int vscale);
int cob_put_num_x(void *p, const cob_desc *d, long long v, int vscale, int opts);
void cob_move(const void *src, const cob_desc *sd, void *dst, const cob_desc *dd);

/* the compiler's descriptors (s32-cobc.c emit_sql_data) */
typedef struct cob_sql_cursor {
    const char *text;           /* the query; SELECT rowid, ... when positioned */
    dbst *st;
    int positioned;             /* its first column is the rowid */
    int open;                   /* 1 open, 2 open and past its last row */
    unsigned rowid_lo, rowid_hi;
    const char *name;
    struct cob_sql_dyn *dyn;    /* DECLARE c CURSOR FOR s: the prepared statement, else 0 */
} cob_sql_cursor;
typedef struct {
    const char *text;
    dbst *st;
    int kind;                   /* 0 run, 1 SELECT INTO, 2 no-op (GRANT/REVOKE), 3 positioned */
    cob_sql_cursor *cur;
} cob_sql_stmt;
enum { K_EXEC = 0, K_SELECT_INTO = 1, K_NOOP = 2, K_POSITIONED = 3 };

/* lp, ld: a VARCHAR's length item (a level-49 pair), else NULL */
typedef struct { void *p; const cob_desc *d; void *ip; const cob_desc *id; void *lp; const cob_desc *ld; } host;
static host g_in[128], g_out[128];
static int g_nin, g_nout;

static sqlite3 *g_db;
static pg_conn *g_pg;                   /* the PostgreSQL connection (COB_SQL_BACKEND=postgres) */
#define DB_OPEN (g_db || g_pg)

/* ---- the backend: SQLite's calls, or pgwire's in their place ------------
 * The runtime is written against SQLite's API; each call goes through one
 * of these.  PostgreSQL answers in SQLite's terms: a step is SQLITE_ROW,
 * SQLITE_DONE or SQLITE_ERROR, and every value is TEXT or NULL (pgwire
 * asks for text), which the runtime converts as it does SQLite's text. */
static int use_pg(void)
{
    const char *b = getenv("COB_SQL_BACKEND");
    return b && (!strcmp(b, "postgres") || !strcmp(b, "postgresql") || !strcmp(b, "pg"));
}
static int db_prepare(const char *t, dbst **st)
{
    if (g_pg) return pg_prepare(g_pg, t, (pg_stmt **)st) == PG_OK ? SQLITE_OK : SQLITE_ERROR;
    return sqlite3_prepare_v2(g_db, t, -1, (sqlite3_stmt **)st, NULL);
}
static int db_step(dbst *st)
{
    if (!g_pg) return sqlite3_step(st);
    int rc = pg_step(st);
    return rc == PG_ROW ? SQLITE_ROW : rc == PG_DONE ? SQLITE_DONE : SQLITE_ERROR;
}
static int db_reset(dbst *st) { return g_pg ? pg_reset(st) : sqlite3_reset(st); }
static int db_clear_bindings(dbst *st) { if (g_pg) { pg_clear_bindings(st); return 0; } return sqlite3_clear_bindings(st); }
static int db_finalize(dbst *st) { if (g_pg) { pg_finalize(st); return 0; } return sqlite3_finalize(st); }
static int db_bind_null(dbst *st, int i) { return g_pg ? pg_bind_text(st, i, NULL, 0) : sqlite3_bind_null(st, i); }
static int db_bind_int64(dbst *st, int i, long long v)
{
    if (!g_pg) return sqlite3_bind_int64(st, i, v);
    char b[24]; snprintf(b, sizeof b, "%lld", v);
    return pg_bind_text(st, i, b, -1);
}
static int db_bind_double(dbst *st, int i, double v)
{
    if (!g_pg) return sqlite3_bind_double(st, i, v);
    char b[32]; snprintf(b, sizeof b, "%.17g", v);
    return pg_bind_text(st, i, b, -1);
}
static int db_bind_text(dbst *st, int i, const char *t, int n)
{
    return g_pg ? pg_bind_text(st, i, t, n) : sqlite3_bind_text(st, i, t, n, SQLITE_TRANSIENT);
}
static int db_bind_parameter_count(dbst *st) { return g_pg ? pg_param_count(st) : sqlite3_bind_parameter_count(st); }
static int db_column_count(dbst *st) { return g_pg ? pg_column_count(st) : sqlite3_column_count(st); }
static int db_column_type(dbst *st, int c)
{
    if (!g_pg) return sqlite3_column_type(st, c);
    return pg_column_text(st, c, NULL) ? SQLITE_TEXT : SQLITE_NULL;
}
static const unsigned char *db_column_text(dbst *st, int c)
{
    return g_pg ? (const unsigned char *)pg_column_text(st, c, NULL) : sqlite3_column_text(st, c);
}
static int db_column_bytes(dbst *st, int c)
{
    if (!g_pg) return sqlite3_column_bytes(st, c);
    int n; pg_column_text(st, c, &n); return n;
}
static long long db_column_int64(dbst *st, int c)
{
    if (!g_pg) return sqlite3_column_int64(st, c);
    const char *t = pg_column_text(st, c, NULL); return t ? atoll(t) : 0;
}
static double db_column_double(dbst *st, int c)
{
    if (!g_pg) return sqlite3_column_double(st, c);
    const char *t = pg_column_text(st, c, NULL); return t ? strtod(t, NULL) : 0;
}
/* a column's declared type, as SQLite gives it: for PostgreSQL, its type's
 * OID named in SQL's words */
static const char *db_column_decltype(dbst *st, int c)
{
    if (!g_pg) return sqlite3_column_decltype(st, c);
    switch (pg_column_type(st, c)) {
    case 16: return "BOOLEAN";
    case 20: return "BIGINT";
    case 21: return "SMALLINT";
    case 23: return "INTEGER";
    case 700: return "REAL";
    case 701: return "DOUBLE PRECISION";
    case 1042: return "CHARACTER";
    case 1043: return "VARCHAR";
    case 1082: return "DATE";
    case 1083: return "TIME";
    case 1114: case 1184: return "TIMESTAMP";
    case 1700: return "NUMERIC";
    default: return "TEXT";
    }
}
static const char *db_column_name(dbst *st, int c) { return g_pg ? pg_column_name(st, c) : sqlite3_column_name(st, c); }
static int db_exec(const char *sql)
{
    if (g_pg) return pg_exec(g_pg, sql) == PG_OK ? SQLITE_OK : SQLITE_ERROR;
    return sqlite3_exec(g_db, sql, 0, 0, 0);
}
static int db_autocommit(void) { return g_pg ? !pg_in_transaction(g_pg) : sqlite3_get_autocommit(g_db); }
static long long db_changes(void) { return g_pg ? pg_changes(g_pg) : sqlite3_changes(g_db); }
static const char *db_errmsg(void) { return g_pg ? pg_errmsg(g_pg) : g_db ? sqlite3_errmsg(g_db) : "no connection"; }
static char g_user[64];
static int g_trace = -1;
static int g_sqlcode;
static char g_sqlstate[6] = "00000";
static char g_errmsg[72];               /* SQLERRMC: the backend's message on an error */
static long long g_rows;                /* SQLERRD(3): the rows the statement touched */

/* every statement and cursor prepared on this connection, to finalize
 * when it closes */
static dbst **g_prepared_slot[4096]; static int g_nprep;
static cob_sql_cursor *g_cursors[512]; static int g_ncur;

/* a completion condition that is a warning (01004): kept only while the
 * statement has nothing worse to say */
static void set_warning(const char *state)
{
    if (!memcmp(g_sqlstate, "00000", 5)) memcpy(g_sqlstate, state, 5);
}

static void set_status(int code, const char *state)
{
    g_sqlcode = code;
    memcpy(g_sqlstate, state, 5); g_sqlstate[5] = 0;
    if (code == 0 || code == 100) g_errmsg[0] = 0;
    else snprintf(g_errmsg, sizeof g_errmsg, "%s", DB_OPEN && code != -305 && code != -811 ? db_errmsg() : "");
}

static void trace_error(const char *what)
{
    if (g_trace < 0) { const char *t = getenv("COB_SQL_TRACE"); g_trace = t && *t && *t != '0'; }
    if (g_trace) fprintf(stderr, "esql: %s: SQLCODE %d SQLSTATE %s: %s\n", what, g_sqlcode, g_sqlstate, db_errmsg());
}

/* the backend's error as SQLCODE and SQLSTATE */
static void set_error(int rc, const char *what)
{
    if (g_pg) {
        /* PostgreSQL says its SQLSTATE; SQLCODE as DB2 would give it */
        const char *st = pg_sqlstate(g_pg);
        int code = !strcmp(st, "23505") ? -803 : !strcmp(st, "23502") ? -407 : !strcmp(st, "23514") ? -545
                 : !strcmp(st, "23503") ? -530 : !strcmp(st, "22003") ? -304 : !strcmp(st, "22019") ? -130
                 : !strncmp(st, "42", 2) ? -204 : !strncmp(st, "22", 2) ? -302 : !strncmp(st, "23", 2) ? -803
                 : !strncmp(st, "08", 2) ? -900 : -1;
        set_status(code, st);
        trace_error(what);
        return;
    }
    int ext = g_db ? sqlite3_extended_errcode(g_db) : rc;
    switch (ext) {
    case SQLITE_CONSTRAINT_UNIQUE: case SQLITE_CONSTRAINT_PRIMARYKEY: set_status(-803, "23000"); break;
    case SQLITE_CONSTRAINT_NOTNULL: set_status(-407, "23000"); break;
    case SQLITE_CONSTRAINT_CHECK: set_status(-545, "23000"); break;
    case SQLITE_CONSTRAINT_FOREIGNKEY: set_status(-530, "23000"); break;
    default:
        /* what SQLite says in words, where it has a SQL-92 condition */
        if (g_db && strstr(sqlite3_errmsg(g_db), "ESCAPE expression must be a single character")) { set_status(-130, "22019"); break; }
        if (g_db && strstr(sqlite3_errmsg(g_db), "integer overflow")) { set_status(-304, "22003"); break; }
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
    int k = 0, i = 0;
    while (i < n && in[i] == ' ') i++;                  /* leading blanks too: a user is a name */
    for (; i < n && in[i] && k < cap - 1; i++) out[k++] = (char)toupper((unsigned char)in[i]);
    while (k && out[k - 1] == ' ') k--;
    out[k] = 0;
}

static void disconnect(void)
{
    if (!DB_OPEN) return;
    for (int i = 0; i < g_nprep; i++) { db_finalize(*g_prepared_slot[i]); *g_prepared_slot[i] = NULL; }
    g_nprep = 0;
    for (int i = 0; i < g_ncur; i++) g_cursors[i]->open = 0;
    g_ncur = 0;
    if (g_pg) {
        if (pg_in_transaction(g_pg)) pg_exec(g_pg, "COMMIT");
        pg_close(g_pg);
        g_pg = NULL;
        return;
    }
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

/* the run ends normally (STOP RUN, or GOBACK from the main program): its
 * open transaction is committed, as DB2 does at normal termination -- ISO
 * leaves it to the implementation.  libcob calls this from cob_stop_run. */
extern void (*cob_at_stop)(void);
static void at_end(void) { disconnect(); }

static int sql_connect(void)
{
    cob_at_stop = at_end;
    if (DB_OPEN) return 1;
    if (use_pg()) {
        /* libpq's variables.  The guest has no DNS: PGHOSTADDR is the IPv4
         * address connected to, PGHOST the name the password file is
         * matched against (either alone serves as both, as in libpq).
         * Without PGPASSWORD, PGPASSFILE or ~/.pgpass.  Each value copied
         * as read: a getenv's result may be overwritten by the next
         * (POSIX), and the guest's is. */
        char host[128], addr[64], port[16], user[64], db[64], pw[256], passfile[512];
        #define ENV_COPY(buf, name, dflt) do { const char *v_ = getenv(name); \
            snprintf(buf, sizeof buf, "%s", v_ && *v_ ? v_ : (dflt)); } while (0)
        ENV_COPY(host, "PGHOST", "");
        ENV_COPY(addr, "PGHOSTADDR", host);
        if (!addr[0]) snprintf(addr, sizeof addr, "127.0.0.1");
        if (!host[0]) snprintf(host, sizeof host, "%s", addr);
        ENV_COPY(port, "PGPORT", "5432");
        ENV_COPY(user, "PGUSER", "postgres");
        ENV_COPY(db, "PGDATABASE", user);
        ENV_COPY(pw, "PGPASSWORD", "");
        ENV_COPY(passfile, "PGPASSFILE", "");
        if (!passfile[0]) { char home[384]; ENV_COPY(home, "HOME", ""); if (home[0]) snprintf(passfile, sizeof passfile, "%s/.pgpass", home); }
        #undef ENV_COPY
        if (!pw[0] && passfile[0]) pg_password_from_file(passfile, host, port, db, user, pw, sizeof pw);
        char err[256];
        if (pg_connect(addr, atoi(port), user, db, pw[0] ? pw : NULL, &g_pg, err, sizeof err) != PG_OK) {
            g_pg = NULL;
            set_status(-1, "08001");
            snprintf(g_errmsg, sizeof g_errmsg, "%s", err);
            if (g_trace < 0) { const char *t = getenv("COB_SQL_TRACE"); g_trace = t && *t && *t != '0'; }
            if (g_trace) fprintf(stderr, "esql: connect: SQLCODE -1 SQLSTATE 08001: %s\n", err);
            return 0;
        }
        return 1;
    }
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
    if (DB_OPEN && strcmp(u, g_user)) disconnect();
    snprintf(g_user, sizeof g_user, "%s", u);
}

/* ---- host variables ---------------------------------------------------- */

void cob_sql_in(void *p, const cob_desc *d, void *ip, const cob_desc *id)
{
    if (g_nin == 128) { fprintf(stderr, "esql: more than 128 input host variables\n"); exit(1); }
    g_in[g_nin].p = p; g_in[g_nin].d = d; g_in[g_nin].ip = ip; g_in[g_nin].id = id;
    g_in[g_nin].lp = NULL; g_in[g_nin].ld = NULL; g_nin++;
}

void cob_sql_out(void *p, const cob_desc *d, void *ip, const cob_desc *id)
{
    if (g_nout == 128) { fprintf(stderr, "esql: more than 128 output host variables\n"); exit(1); }
    g_out[g_nout].p = p; g_out[g_nout].d = d; g_out[g_nout].ip = ip; g_out[g_nout].id = id;
    g_out[g_nout].lp = NULL; g_out[g_nout].ld = NULL; g_nout++;
}

/* a VARCHAR host variable (DB2's: a group of a level-49 length and a
 * level-49 text): in, text(1:length) exactly; out, the text and its length */
void cob_sql_in_vc(void *p, const cob_desc *d, void *ip, const cob_desc *id, void *lp, const cob_desc *ld)
{
    cob_sql_in(p, d, ip, id);
    g_in[g_nin - 1].lp = lp; g_in[g_nin - 1].ld = ld;
}
void cob_sql_out_vc(void *p, const cob_desc *d, void *ip, const cob_desc *id, void *lp, const cob_desc *ld)
{
    cob_sql_out(p, d, ip, id);
    g_out[g_nout - 1].lp = lp; g_out[g_nout - 1].ld = ld;
}

static void clear_hosts(void) { g_nin = g_nout = 0; }

/* SQL descriptor areas (below): the next statement's USING and INTO */
typedef struct desc_area desc_area;
static desc_area *g_desc_in, *g_desc_out;
static int bind_desc(dbst *st);
static int store_desc(dbst *st, int base);

static const char *strcasestr_simple(const char *h, const char *n)
{
    size_t l = strlen(n);
    for (; *h; h++) if (!strncasecmp(h, n, l)) return h;
    return NULL;
}

static const long long p10[19] = { 1LL, 10LL, 100LL, 1000LL, 10000LL, 100000LL, 1000000LL, 10000000LL, 100000000LL,
    1000000000LL, 10000000000LL, 100000000000LL, 1000000000000LL, 10000000000000LL, 100000000000000LL,
    1000000000000000LL, 10000000000000000LL, 100000000000000000LL, 1000000000000000000LL };

/* one input: NULL by a negative indicator; a numeric item as an integer
 * or its exact decimal text; anything else as text, trailing spaces
 * trimmed (SQL-92 compares CHAR blank-padded, SQLite exactly) */
static double host_dbl(const host *h)
{
    if (h->d->size == 4) { float f; memcpy(&f, h->p, 4); return f; }
    double x; memcpy(&x, h->p, 8); return x;
}
static void host_set_dbl(const host *h, double x)
{
    if (h->d->size == 4) { float f = (float)x; memcpy(h->p, &f, 4); }
    else memcpy(h->p, &x, 8);
}

static int bind_in(dbst *st, int i, const host *h)
{
    if (h->ip && cob_get_num(h->ip, h->id) < 0) return db_bind_null(st, i);
    const cob_desc *d = h->d;
    if (h->lp) {                                    /* a VARCHAR: its length's bytes, nothing trimmed */
        long long n = cob_get_num(h->lp, h->ld);
        if (n < 0) n = 0;
        if (n > (long long)d->size) n = d->size;
        return db_bind_text(st, i, h->p, (int)n);
    }
    if (d->usage == COB_U_FLOAT) return db_bind_double(st, i, host_dbl(h));   /* COMP-1/COMP-2: REAL (docs/usage.md) */
    if (d->cat == COB_NUM) {
        long long v = cob_get_num(h->p, d);
        int sc = d->scale;
        if (sc <= 0) {
            for (int k = 0; k < -sc && k < 18; k++) v *= 10;
            return db_bind_int64(st, i, v);
        }
        char buf[48];
        unsigned long long m = v < 0 ? 0 - (unsigned long long)v : (unsigned long long)v;
        unsigned long long ip = m / (unsigned long long)p10[sc > 18 ? 18 : sc], fp = m % (unsigned long long)p10[sc > 18 ? 18 : sc];
        snprintf(buf, sizeof buf, "%s%llu.%0*llu", v < 0 ? "-" : "", ip, sc, fp);
        return db_bind_text(st, i, buf, -1);
    }
    int n = (int)d->size;
    const char *s = h->p;
    while (n && s[n - 1] == ' ') n--;
    return db_bind_text(st, i, s, n);
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
/* one value (SQLite's type, its integer, its text) to a host variable:
 * NULL to the indicator (or an error), a number by its exact text,
 * anything else by the MOVE rules */
static int store_val(int ty, long long iv, const char *t, int n, const host *h)
{
    if (ty == SQLITE_NULL) {
        if (!h->ip) { set_status(-305, "22002"); trace_error("fetch: NULL with no indicator"); return 0; }
        cob_put_num(h->ip, h->id, -1, 0);
        return 1;
    }
    if (h->ip) cob_put_num(h->ip, h->id, 0, 0);
    const cob_desc *d = h->d;
    if (d->usage == COB_U_FLOAT) {                 /* a float host variable: the value, as near as it holds */
        char *e = NULL; double x = ty == SQLITE_INTEGER ? (double)iv : strtod(t ? t : "", &e);
        if (ty != SQLITE_INTEGER && (!t || e == t)) { set_status(-302, "22018"); trace_error("fetch: not a number"); return 0; }
        host_set_dbl(h, x);
        return 1;
    }
    if (d->cat == COB_NUM) {
        long long v; int sc;
        if (ty == SQLITE_INTEGER) { v = iv; sc = 0; }
        else {
            int r = parse_decimal(t ? t : "", &v, &sc);
            if (r <= 0) { set_status(r < 0 ? -304 : -302, r < 0 ? "22003" : "22018"); trace_error("fetch: not a number"); return 0; }
        }
        /* too many integer digits for the host variable: 22003, not the
         * silent high-order truncation of a MOVE; the lost fraction
         * digits are the implementation's (SQL-92: rounding or truncation) */
        if (cob_put_num_x(h->p, d, v, sc, 2)) { set_status(-304, "22003"); trace_error("fetch: value out of range"); return 0; }
        return 1;
    }
    cob_desc sd; memset(&sd, 0, sizeof sd);
    sd.cat = COB_ALNUM; sd.size = (unsigned)n;
    if (n) cob_move(t, &sd, h->p, d);
    else memset(h->p, ' ', d->size);
    if (h->lp) cob_put_num(h->lp, h->ld, n < (int)d->size ? n : (int)d->size, 0);   /* a VARCHAR: the bytes it holds */
    if (n > (int)d->size) {
        /* SQL-92: string data, right truncation, a warning; the indicator
         * holds the length the value had */
        int k = n; while (k > (int)d->size && t[k - 1] == ' ') k--;
        if (k > (int)d->size) {
            set_warning("01004");
            if (h->ip) cob_put_num(h->ip, h->id, n, 0);
        }
    }
    return 1;
}

static int store_out(dbst *st, int c, const host *h)
{
    int ty = db_column_type(st, c);
    if (h->d->usage == COB_U_FLOAT && (ty == SQLITE_FLOAT || ty == SQLITE_INTEGER)) {   /* to a float: the double itself, not its text */
        if (h->ip) cob_put_num(h->ip, h->id, 0, 0);
        host_set_dbl(h, db_column_double(st, c));
        return 1;
    }
    const char *t = ty == SQLITE_NULL ? NULL : (const char *)db_column_text(st, c);
    return store_val(ty, ty == SQLITE_INTEGER ? db_column_int64(st, c) : 0, t, t ? db_column_bytes(st, c) : 0, h);
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
            /* dropped, not main.: a view stored with main.X in its text
             * cannot be attached under its schema's name again ("cannot
             * reference objects in database main"), which took HU away
             * from every later program; unqualified, a statement finds
             * main first and a view resolves in its own database */
            i += ul + 1;
            continue;
        }
        out[o++] = text[i++];
    }
    out[o] = 0;
    return out;
}

/* CREATE TABLE and ALTER TABLE ... ADD: a character column compares
 * blank-padded, as SQL-92's PAD SPACE collations do, through SQLite's own
 * RTRIM collation -- a column definition's type (its second word, at the
 * top of the element list) gets COLLATE RTRIM unless it names a collation.
 * tests/nist-sql-schema.py does the same for the loaded schemas.  Only
 * column definitions: a CAST inside a CHECK takes no COLLATE. */
static int ddl_word(const char *t, size_t *i, char *w, int wn)
{
    size_t k = *i; int n = 0;
    while (t[k] == ' ' || t[k] == '\t' || t[k] == '\n') k++;
    *i = k;
    while ((isalnum((unsigned char)t[k]) || t[k] == '_') && n < wn - 1) w[n++] = (char)tolower((unsigned char)t[k++]);
    w[n] = 0;
    return n;
}
static int is_char_type(const char *w)
{
    return !strcmp(w, "char") || !strcmp(w, "character") || !strcmp(w, "varchar") || !strcmp(w, "nchar") ||
           !strcmp(w, "national");
}
static char *ddl_collate(const char *text)
{
    size_t n = strlen(text), o = 0, i = 0;
    char w[32], w2[32];
    size_t j = 0, j2;
    ddl_word(text, &j, w, sizeof w);
    j2 = j + strlen(w);
    ddl_word(text, &j2, w2, sizeof w2);
    int create = !strcmp(w, "create") && (!strcmp(w2, "table") || !strcmp(w2, "global") || !strcmp(w2, "local"));
    int alter = !strcmp(w, "alter") && !strcmp(w2, "table");
    if (!create && !alter) return NULL;
    char *out = malloc(n * 2 + 64);
    int depth = 0, elem_word = -1;              /* the word number within a column element, -1 outside one */
    char quote = 0;
    while (i < n) {
        char c = text[i];
        if (quote) { if (c == quote) quote = 0; out[o++] = text[i++]; continue; }
        if (c == '\'' || c == '"') { quote = c; out[o++] = text[i++]; continue; }
        if (c == '(') { depth++; if (create && depth == 1) elem_word = 0; out[o++] = text[i++]; continue; }
        if (c == ')') { depth--; out[o++] = text[i++]; continue; }
        if (c == ',' && depth == 1 && create) { elem_word = 0; out[o++] = text[i++]; continue; }
        if (isalpha((unsigned char)c) && (i == 0 || !(isalnum((unsigned char)text[i - 1]) || text[i - 1] == '_'))) {
            size_t k = i; char word[32];
            ddl_word(text, &k, word, sizeof word);
            size_t wl = strlen(word);
            /* ALTER TABLE t ADD [COLUMN] name type: the element starts after ADD */
            if (alter && depth == 0 && !strcmp(word, "add")) { elem_word = 0; memcpy(out + o, text + i, wl); o += wl; i += wl; continue; }
            else if (alter && depth == 0 && !strcmp(word, "column") && elem_word == 0) { memcpy(out + o, text + i, wl); o += wl; i += wl; continue; }
            int at_type = elem_word == 1 && ((create && depth == 1) || (alter && depth == 0));
            if (elem_word >= 0 && ((create && depth == 1) || (alter && depth == 0))) elem_word++;
            memcpy(out + o, text + i, wl); o += wl; i += wl;
            if (at_type && is_char_type(word)) {
                /* the rest of the type: CHARACTER VARYING, NATIONAL CHARACTER [VARYING], (n) */
                for (;;) {
                    size_t k2 = i; char nx[32];
                    ddl_word(text, &k2, nx, sizeof nx);
                    if (!strcmp(nx, "character") || !strcmp(nx, "char") || !strcmp(nx, "varying")) {
                        while (i < k2 + strlen(nx)) out[o++] = text[i++];
                        continue;
                    }
                    break;
                }
                size_t k3 = i; while (text[k3] == ' ') k3++;
                if (text[k3] == '(') { while (i <= k3) out[o++] = text[i++]; while (i < n && text[i] != ')') out[o++] = text[i++]; if (i < n) out[o++] = text[i++]; }
                size_t k4 = i; char nx[32]; ddl_word(text, &k4, nx, sizeof nx);
                if (strcmp(nx, "collate") && strcmp(nx, "set")) { memcpy(out + o, " COLLATE RTRIM", 14); o += 14; }
            }
            continue;
        }
        out[o++] = text[i++];
    }
    out[o] = 0;
    return out;
}

static int prepare_x(dbst **slot, const char *text, int reg)
{
    if (*slot) return 1;
    /* SQLite's: the user's own qualifier to main, COLLATE RTRIM on
     * character columns.  PostgreSQL has schemas and PAD SPACE itself. */
    char *t = g_pg ? strdup(text) : own_schema(text);
    char *d = g_pg ? NULL : ddl_collate(t);
    if (d) { free(t); t = d; }
    int rc = db_prepare(t, slot);
    if (rc != SQLITE_OK) { set_error(rc, t); free(t); *slot = NULL; return 0; }   /* the text as SQLite saw it */
    free(t);
    if (reg && g_nprep < 4096) g_prepared_slot[g_nprep++] = slot;
    return 1;
}
static int prepare(dbst **slot, const char *text) { return prepare_x(slot, text, 1); }

/* SQL-92: a transaction begins with the first statement after the last
 * one ended */
static void begin_if_needed(void)
{
    if (db_autocommit()) db_exec("BEGIN");
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
    for (int i = 0; i < g_ncur; i++) if (g_cursors[i]->open) { db_reset(g_cursors[i]->st); g_cursors[i]->open = 0; }
    g_ncur = 0;
    if (!db_autocommit()) {
        int rc = db_exec(commit ? "COMMIT" : "ROLLBACK");
        if (rc != SQLITE_OK) { set_error(rc, commit ? "COMMIT" : "ROLLBACK"); return g_sqlcode; }
    }
    return 0;
}

/* an input host variable's value as text, trimmed */
static void host_text(const host *h, char *out, int cap)
{
    int n = (int)h->d->size; const char *s = h->p;
    if (h->d->usage == COB_U_FLOAT) { snprintf(out, (size_t)cap, "%.17g", host_dbl(h)); return; }
    if (h->d->cat == COB_NUM) { snprintf(out, (size_t)cap, "%lld", cob_get_num(h->p, h->d)); return; }
    while (n && s[n - 1] == ' ') n--;
    if (n > cap - 1) n = cap - 1;
    memcpy(out, s, (size_t)n); out[n] = 0;
}

/* CONNECT [TO target] [AS name] [USER user [USING pw]] | CONNECT RESET |
 * CONNECT :user IDENTIFIED BY :pw | DISCONNECT [...]: the authorization
 * id is the user (a word or a host variable); the target, the name and
 * the password are taken and not used -- there is one database directory */
static int connect_stmt(const char *text)
{
    set_status(0, "00000");
    const char *p = text; char w[64];
    int in = 0;
    if (first_word(text, "disconnect")) { disconnect(); return 0; }
    while (*p == ' ') p++;
    p += 7;                                     /* connect */
    char user[64] = "";
    for (;;) {
        while (*p == ' ' || *p == ',') p++;
        if (!*p) break;
        if (*p == '?') { p++; in++; continue; }       /* a TO or AS host variable: not used */
        if (*p == '\'' ) { const char *e = strchr(p + 1, '\''); p = e ? e + 1 : p + strlen(p); continue; }
        int n = 0; while (*p && *p != ' ' && *p != ',' && n < 63) w[n++] = (char)tolower((unsigned char)*p++); w[n] = 0;
        if (!strcmp(w, "reset")) { disconnect(); return 0; }
        if (!strcmp(w, "user") || !strcmp(w, "identified")) {
            if (!strcmp(w, "identified")) break;        /* :user IDENTIFIED BY :pw: the user came first */
            while (*p == ' ') p++;
            if (*p == '?') { if (in < g_nin) host_text(&g_in[in], user, sizeof user); in++; p++; }
            else if (*p == '\'') { const char *e = strchr(p + 1, '\''); int l = e ? (int)(e - p - 1) : 0; if (l > 63) l = 63; memcpy(user, p + 1, (size_t)l); user[l] = 0; p = e ? e + 1 : p + strlen(p); }
            else { int k = 0; while (*p && *p != ' ' && k < 63) user[k++] = *p++; user[k] = 0; }
        }
    }
    /* CONNECT :user IDENTIFIED BY :pw (Oracle): the first host variable */
    if (!user[0] && strcasestr_simple(text, "identified") && g_nin) host_text(&g_in[0], user, sizeof user);
    if (user[0]) {
        char u[64]; upcase_trim(u, user, (int)strlen(user), sizeof u);
        if (DB_OPEN && strcmp(u, g_user)) disconnect();
        snprintf(g_user, sizeof g_user, "%s", u);
    }
    return sql_connect() ? 0 : g_sqlcode;
}

/* ---- the diagnostics area (GET DIAGNOSTICS) ----------------------------
 * What the last statement other than GET DIAGNOSTICS left: its condition,
 * the rows it touched, its command function (ISO 9075's names). */
static struct {
    int number;                 /* conditions: 0 on success, else 1 */
    char state[6];
    char msg[256];
    long long rows;
    char cmd[48], dyncmd[48];
} g_diag = { 0, "00000", "", 0, "", "" };

/* ISO's command function for a statement's text */
static void command_function(const char *text, char *out, int cap, int positioned)
{
    const char *p = text; char w1[32] = "", w2[32] = "";
    int n = 0;
    while (*p == ' ') p++;
    while (isalpha((unsigned char)*p) && n < 31) w1[n++] = (char)toupper((unsigned char)*p++); w1[n] = 0;
    while (*p == ' ') p++;
    n = 0; while ((isalpha((unsigned char)*p) || *p == '_') && n < 31) w2[n++] = (char)toupper((unsigned char)*p++); w2[n] = 0;
    if (!strcmp(w1, "UPDATE") || !strcmp(w1, "DELETE")) snprintf(out, (size_t)cap, "%s %s", w1, positioned ? "CURSOR" : "WHERE");
    else if (!strcmp(w1, "COMMIT") || !strcmp(w1, "ROLLBACK")) snprintf(out, (size_t)cap, "%s WORK", w1);
    else if (!strcmp(w1, "CREATE") || !strcmp(w1, "DROP") || !strcmp(w1, "ALTER") || !strcmp(w1, "ALLOCATE") ||
             !strcmp(w1, "DEALLOCATE") || !strcmp(w1, "GET") || !strcmp(w1, "DESCRIBE")) snprintf(out, (size_t)cap, "%s %s", w1, w2);
    else if (!strcmp(w1, "SET")) {
        if (!strcmp(w2, "SESSION")) snprintf(out, (size_t)cap, "SET SESSION AUTHORIZATION");
        else if (!strcmp(w2, "CONSTRAINTS")) snprintf(out, (size_t)cap, "SET CONSTRAINT");
        else if (!strcmp(w2, "TIME")) snprintf(out, (size_t)cap, "SET TIME ZONE");
        else snprintf(out, (size_t)cap, "SET %s", w2);
    }
    else snprintf(out, (size_t)cap, "%s", w1);
}

/* the statement is over: its diagnostics are the area's */
static void diag_end(const char *cmd, const char *dyncmd)
{
    g_diag.number = memcmp(g_sqlstate, "00000", 5) ? 1 : 0;
    memcpy(g_diag.state, g_sqlstate, 6);
    snprintf(g_diag.msg, sizeof g_diag.msg, "%s", g_errmsg);
    g_diag.rows = g_rows;
    snprintf(g_diag.cmd, sizeof g_diag.cmd, "%s", cmd);
    snprintf(g_diag.dyncmd, sizeof g_diag.dyncmd, "%s", dyncmd ? dyncmd : "");
}

/* SET SESSION AUTHORIZATION value | SET TRANSACTION | SET CONSTRAINTS |
 * SET TIME ZONE | SET CATALOG | SET SCHEMA | SET NAMES: the authorization
 * changes the user; the rest succeed and do nothing -- SQLite's
 * transactions are serializable and read-write, its constraints are not
 * deferred by statement, and it has no time zones, catalogs or character
 * sets (behavior points, docs/esql.md) */
static int set_stmt(const char *text)
{
    const char *p = text; char w[32]; int n;
    while (*p == ' ') p++;
    p += 3; while (*p == ' ') p++;
    n = 0; while (isalpha((unsigned char)*p) && n < 31) w[n++] = (char)tolower((unsigned char)*p++); w[n] = 0;
    if (!strcmp(w, "session")) {
        const char *a = strcasestr_simple(p, "authorization");
        if (!a) { set_status(-104, "42000"); return g_sqlcode; }
        a += 13; while (*a == ' ') a++;
        char user[64] = "";
        if (*a == '?' && g_nin) host_text(&g_in[0], user, sizeof user);
        else if (*a == '\'') { const char *e = strchr(a + 1, '\''); int l = e ? (int)(e - a - 1) : 0; if (l > 63) l = 63; memcpy(user, a + 1, (size_t)l); user[l] = 0; }
        else { int k = 0; while (*a && *a != ' ' && k < 63) user[k++] = *a++; user[k] = 0; }
        char u[64]; upcase_trim(u, user, (int)strlen(user), sizeof u);
        if (!u[0]) { set_status(-104, "28000"); return g_sqlcode; }
        if (DB_OPEN && !db_autocommit()) { set_status(-918, "25000"); return g_sqlcode; }   /* not in a transaction (SQL-92) */
        if (DB_OPEN && strcmp(u, g_user)) disconnect();
        snprintf(g_user, sizeof g_user, "%s", u);
        return sql_connect() ? 0 : g_sqlcode;
    }
    return 0;
}

/* one statement: the static ones, EXECUTE IMMEDIATE's and EXECUTE's.
 * slot caches the prepared statement when reg is set; dynamic says a
 * statement returning a row delivers it to the outputs, as SELECT INTO */
static int run_stmt(dbst **slot, const char *text, int kind, cob_sql_cursor *cur, int reg, int dynamic)
{
    set_status(0, "00000");
    g_rows = 0;
    if (kind == K_NOOP || first_word(text, "grant") || first_word(text, "revoke")) { clear_hosts(); return 0; }   /* SQLite has no privileges */
    if (first_word(text, "connect") || first_word(text, "disconnect")) {
        int r = connect_stmt(text);
        clear_hosts();
        return r;
    }
    if (first_word(text, "set")) { int r = set_stmt(text); clear_hosts(); return r; }
    if (!sql_connect()) { clear_hosts(); return g_sqlcode; }
    if (first_word(text, "commit") || first_word(text, "rollback")) {
        clear_hosts();
        return end_transaction(first_word(text, "commit"));
    }
    begin_if_needed();
    if (!prepare_x(slot, text, reg)) { clear_hosts(); return g_sqlcode; }
    dbst *st = *slot;
    int i = 0;
    if (g_desc_in) { if (!bind_desc(st)) goto done; }
    else for (; i < g_nin; i++) bind_in(st, i + 1, &g_in[i]);
    if (kind == K_POSITIONED) {
        cob_sql_cursor *c = cur;
        if (!c->open) { set_status(-508, "24000"); trace_error("positioned: cursor not open"); goto done; }
        db_bind_int64(st, i + 1, (long long)(((unsigned long long)c->rowid_hi << 32) | c->rowid_lo));
    }
    int rc = db_step(st);
    if (kind == K_SELECT_INTO || (dynamic && db_column_count(st) > 0)) {
        if (rc == SQLITE_ROW) {
            g_rows = 1;
            if (g_desc_out) { if (!store_desc(st, 0)) goto done; }
            else for (int k = 0; k < g_nout && k < db_column_count(st); k++) if (!store_out(st, k, &g_out[k])) goto done;
            if (db_step(st) == SQLITE_ROW) { set_status(-811, "21000"); trace_error("SELECT INTO: more than one row"); }
        } else if (rc == SQLITE_DONE) set_status(100, "02000");
        else set_error(rc, text);
    } else {
        while (rc == SQLITE_ROW) rc = db_step(st);
        if (rc != SQLITE_DONE) set_error(rc, text);
        else if (first_word(text, "update") || first_word(text, "delete") || first_word(text, "insert")) {
            g_rows = db_changes();
            if (g_rows == 0) set_status(100, "02000");                      /* no row: no data (SQL-92) */
        }
    }
done:
    db_reset(st);
    db_clear_bindings(st);
    clear_hosts();
    g_desc_in = g_desc_out = NULL;
    return g_sqlcode;
}

int cob_sql_exec(cob_sql_stmt *s)
{
    int r = run_stmt(&s->st, s->text, s->kind, s->cur, 1, 0);
    char cmd[48]; command_function(s->text, cmd, sizeof cmd, s->kind == K_POSITIONED);
    diag_end(cmd, NULL);
    return r;
}

/* ---- dynamic SQL -------------------------------------------------------- */

/* a prepared statement's descriptor (s32-cobc.c emit_sql_data) */
typedef struct cob_sql_dyn {
    const char *name;
    dbst *st;
    char *text;                 /* the prepared text, the runtime's copy */
    int rowid;                  /* a positioned cursor runs it: its SELECT gets rowid first */
} cob_sql_dyn;

/* the text of a statement given as a host variable or a literal: the
 * first input, all of it (a host variable holds the statement) */
static char *dyn_text(void)
{
    if (!g_nin) return NULL;
    const host *h = &g_in[0];
    int n = (int)h->d->size;
    const char *s = h->p;
    while (n && s[n - 1] == ' ') n--;
    char *t = malloc((size_t)n + 1);
    memcpy(t, s, (size_t)n); t[n] = 0;
    return t;
}

/* EXECUTE IMMEDIATE :text | 'text': the text from the one input, or a
 * literal's (cob_sql_immediate_text) */
static int immediate(char *t);
int cob_sql_immediate(void)
{
    char *t = dyn_text();
    clear_hosts();
    return immediate(t);
}
int cob_sql_immediate_text(const char *lit)
{
    size_t n = strlen(lit); char *t = malloc(n + 1); memcpy(t, lit, n + 1);
    clear_hosts();
    return immediate(t);
}
static int immediate(char *t)
{
    int r = 0;
    if (!t || !*t) { set_status(-104, "42000"); r = g_sqlcode; }
    else {
        dbst *st = NULL;
        r = run_stmt(&st, t, K_EXEC, NULL, 0, 0);
        if (st) db_finalize(st);
    }
    char dc[48]; command_function(t ? t : "", dc, sizeof dc, 0);
    diag_end("EXECUTE IMMEDIATE", dc);
    free(t);
    return r;
}

/* PREPARE name FROM :text | 'text' */
static int prepare_dyn(cob_sql_dyn *d, char *t);
int cob_sql_prepare(cob_sql_dyn *d)
{
    char *t = dyn_text();
    clear_hosts();
    return prepare_dyn(d, t);
}
int cob_sql_prepare_text(cob_sql_dyn *d, const char *lit)
{
    size_t n = strlen(lit); char *t = malloc(n + 1); memcpy(t, lit, n + 1);
    clear_hosts();
    return prepare_dyn(d, t);
}
static int prepare_dyn(cob_sql_dyn *d, char *t)
{
    if (t && d->rowid) {
        /* SELECT rowid, ... for WHERE CURRENT OF, as the compiler does a
         * static cursor's query */
        const char *q = t; while (*q == ' ') q++;
        if (!strncasecmp(q, "select", 6) && !isalnum((unsigned char)q[6])) {
            size_t off = (size_t)(q - t) + 6, n = strlen(t);
            char *r = malloc(n + 16);
            snprintf(r, n + 16, "%.*s rowid,%s", (int)off, t, t + off);
            free(t); t = r;
        }
    }
    set_status(0, "00000");
    g_rows = 0;
    if (d->st) {
        /* a cursor open on the old statement is closed with it */
        for (int k = 0; k < g_ncur; k++) if (g_cursors[k]->dyn == d && g_cursors[k]->open) { g_cursors[k]->open = 0; g_cursors[k]->st = NULL; }
        for (int k = 0; k < g_nprep; k++) if (g_prepared_slot[k] == &d->st) g_prepared_slot[k] = g_prepared_slot[--g_nprep];
        db_finalize(d->st); d->st = NULL;
    }
    free(d->text); d->text = t;
    if (!t || !*t) set_status(-104, "42000");
    else if (sql_connect()) {
        /* statements the runtime carries out itself are kept as text */
        if (!(first_word(t, "commit") || first_word(t, "rollback") || first_word(t, "set") || first_word(t, "grant") ||
              first_word(t, "revoke") || first_word(t, "connect") || first_word(t, "disconnect"))) {
            begin_if_needed();
            prepare(&d->st, t);
        }
    }
    diag_end("PREPARE", NULL);
    return g_sqlcode;
}

/* EXECUTE name [USING ...] [INTO ...] */
int cob_sql_execute(cob_sql_dyn *d)
{
    int r;
    if (!d->text) { clear_hosts(); set_status(-518, "26000"); trace_error(d->name); r = g_sqlcode; }   /* not prepared */
    else r = run_stmt(&d->st, d->text, K_EXEC, NULL, 1, 1);
    char dc[48]; command_function(d->text ? d->text : "", dc, sizeof dc, 0);
    diag_end("EXECUTE", dc);
    return r;
}

/* DEALLOCATE PREPARE name */
int cob_sql_deallocate_prepare(cob_sql_dyn *d)
{
    set_status(0, "00000");
    if (!d->text) set_status(-514, "26000");
    if (d->st) db_finalize(d->st);
    for (int i = 0; i < g_nprep; i++) if (g_prepared_slot[i] == &d->st) g_prepared_slot[i] = g_prepared_slot[--g_nprep];
    d->st = NULL; free(d->text); d->text = NULL;
    clear_hosts();
    diag_end("DEALLOCATE PREPARE", NULL);
    return g_sqlcode;
}

/* ---- GET DIAGNOSTICS ----------------------------------------------------- */

static int g_diag_cond = 1;

void cob_sql_diag_cond(void *p, const cob_desc *d) { g_diag_cond = (int)cob_get_num(p, d); }
void cob_sql_diag_cond_n(int n) { g_diag_cond = n; }

/* one GET DIAGNOSTICS target: statement items 0 NUMBER, 1 MORE,
 * 2 COMMAND_FUNCTION, 3 DYNAMIC_FUNCTION, 4 ROW_COUNT; condition items
 * (of condition g_diag_cond) 10 RETURNED_SQLSTATE, 11 MESSAGE_TEXT,
 * 12 MESSAGE_LENGTH, 13 MESSAGE_OCTET_LENGTH, 14 CLASS_ORIGIN,
 * 15 SUBCLASS_ORIGIN, 16 CONDITION_NUMBER, 17 anything else (blank) */
void cob_sql_diag(void *p, const cob_desc *d, int item)
{
    char buf[300] = ""; long long v = 0; int num = 0;
    int cond_ok = g_diag_cond >= 1 && g_diag_cond <= g_diag.number;
    switch (item) {
    case 0: v = g_diag.number; num = 1; break;
    case 1: snprintf(buf, sizeof buf, "N"); break;
    case 2: snprintf(buf, sizeof buf, "%s", g_diag.cmd); break;
    case 3: snprintf(buf, sizeof buf, "%s", g_diag.dyncmd); break;
    case 4: v = g_diag.rows; num = 1; break;
    case 10: if (cond_ok) snprintf(buf, sizeof buf, "%s", g_diag.state); break;
    case 11: if (cond_ok) snprintf(buf, sizeof buf, "%s", g_diag.msg); break;
    case 12: case 13: v = cond_ok ? (long long)strlen(g_diag.msg) : 0; num = 1; break;
    case 14: case 15:
        /* ISO 9075 for the standard's classes and subclasses */
        if (cond_ok) snprintf(buf, sizeof buf, "%s", "ISO 9075");
        break;
    case 16: v = cond_ok ? g_diag_cond : 0; num = 1; break;
    default: break;
    }
    if (num || d->cat == COB_NUM) { if (d->cat == COB_NUM) cob_put_num(p, d, num ? v : 0, 0); }
    else {
        cob_desc sd; memset(&sd, 0, sizeof sd); sd.cat = COB_ALNUM; sd.size = (unsigned)strlen(buf);
        if (sd.size) cob_move(buf, &sd, p, d); else memset(p, ' ', d->size);
    }
    set_status(0, "00000");     /* GET DIAGNOSTICS itself succeeds, and leaves the area as it was */
}

int cob_sql_open(cob_sql_cursor *c)
{
    set_status(0, "00000");
    g_rows = 0;
    if (!sql_connect()) { clear_hosts(); diag_end("OPEN", NULL); return g_sqlcode; }
    if (c->open) { set_status(-502, "24000"); trace_error(c->name); clear_hosts(); diag_end("OPEN", NULL); return g_sqlcode; }
    if (g_pg && c->positioned) {
        /* positioned UPDATE and DELETE go through SQLite's rowid here */
        set_status(-1, "0A000");
        snprintf(g_errmsg, sizeof g_errmsg, "a cursor for positioned UPDATE or DELETE is not implemented for PostgreSQL");
        trace_error(c->name); clear_hosts(); diag_end("OPEN", NULL); return g_sqlcode;
    }
    begin_if_needed();
    if (c->dyn) {
        /* DECLARE c CURSOR FOR s: s's prepared statement */
        if (!c->dyn->st) { set_status(-518, "26000"); trace_error(c->name); clear_hosts(); diag_end("OPEN", NULL); return g_sqlcode; }
        c->st = c->dyn->st;
    } else if (!prepare(&c->st, c->text)) { clear_hosts(); diag_end("OPEN", NULL); return g_sqlcode; }
    db_reset(c->st); db_clear_bindings(c->st);
    if (g_desc_in) { if (!bind_desc(c->st)) { clear_hosts(); diag_end("OPEN", NULL); return g_sqlcode; } }
    else for (int i = 0; i < g_nin; i++) bind_in(c->st, i + 1, &g_in[i]);
    clear_hosts();
    c->open = 1;
    if (g_ncur < 512) g_cursors[g_ncur++] = c;
    diag_end("OPEN", NULL);
    return 0;
}

int cob_sql_fetch(cob_sql_cursor *c)
{
    set_status(0, "00000");
    g_rows = 0;
    if (!c->open || !c->st) { set_status(-501, "24000"); trace_error(c->name); clear_hosts(); diag_end("FETCH", NULL); return g_sqlcode; }
    /* past the last row, no data again: a step after SQLITE_DONE would
     * start the query over */
    if (c->open == 2) { set_status(100, "02000"); clear_hosts(); diag_end("FETCH", NULL); return g_sqlcode; }
    int rc = db_step(c->st);
    if (rc == SQLITE_ROW) {
        int base = 0;
        g_rows = 1;
        if (c->positioned) {
            unsigned long long r = (unsigned long long)db_column_int64(c->st, 0);
            c->rowid_lo = (unsigned)r; c->rowid_hi = (unsigned)(r >> 32);
            base = 1;
        }
        if (g_desc_out) store_desc(c->st, base);
        else for (int k = 0; k < g_nout && base + k < db_column_count(c->st); k++)
            if (!store_out(c->st, base + k, &g_out[k])) break;
    } else if (rc == SQLITE_DONE) { set_status(100, "02000"); c->open = 2; }
    else set_error(rc, c->name);
    clear_hosts();
    g_desc_out = NULL;
    diag_end("FETCH", NULL);
    return g_sqlcode;
}

int cob_sql_close(cob_sql_cursor *c)
{
    set_status(0, "00000");
    g_rows = 0;
    clear_hosts();
    if (!c->open) { set_status(-501, "24000"); trace_error(c->name); diag_end("CLOSE", NULL); return g_sqlcode; }
    db_reset(c->st);
    c->open = 0;
    diag_end("CLOSE", NULL);
    return 0;
}

/* ---- SQL descriptor areas (ISO 9075 dynamic SQL) -----------------------
 * ALLOCATE DESCRIPTOR name [WITH MAX n]; SET and GET DESCRIPTOR, COUNT
 * and per VALUE n the fields below; DESCRIBE [INPUT | OUTPUT] s USING SQL
 * DESCRIPTOR; EXECUTE ... USING / INTO, OPEN ... USING and FETCH ... INTO
 * SQL DESCRIPTOR.  An item's value is SQLite's: NULL, an integer, or text
 * (a REAL as SQLite prints it). */
enum { DF_COUNT = 0, DF_TYPE, DF_LENGTH, DF_OCTET_LENGTH, DF_RETURNED_LENGTH, DF_RETURNED_OCTET_LENGTH,
       DF_PRECISION, DF_SCALE, DF_DI_CODE, DF_DI_PRECISION, DF_NULLABLE, DF_INDICATOR, DF_DATA,
       DF_NAME, DF_UNNAMED, DF_OTHER };
typedef struct {
    int type, length, octet, precision, scale, dicode, diprec, nullable, unnamed, indicator;
    char name[132];
    int vty; long long iv; char *tv; int tn;        /* the value: SQLite's type, integer, text */
} desc_item;
struct desc_area { char name[132]; int max, count; desc_item *it; };
static desc_area *g_descs[64]; static int g_ndesc;
static desc_area *g_dcur;                           /* the statement's descriptor */
static desc_item *g_ditem;                          /* its VALUE n */
static int g_dbad;                                  /* the statement already failed */
/* g_desc_in, g_desc_out: USING / INTO SQL DESCRIPTOR for the next statement (declared above) */

static desc_area *desc_find(const char *name)
{
    for (int i = 0; i < g_ndesc; i++) if (!strcmp(g_descs[i]->name, name)) return g_descs[i];
    return NULL;
}

/* the descriptor's name: a literal, or the one pending input */
static void desc_name(const char *lit, char *out)
{
    if (lit) snprintf(out, 132, "%s", lit);
    else if (g_nin) host_text(&g_in[0], out, 132);
    else out[0] = 0;
    int n = (int)strlen(out); while (n && out[n - 1] == ' ') out[--n] = 0;
    clear_hosts();
}

static void desc_fail(int code, const char *state) { if (!g_dbad) { set_status(code, state); trace_error("descriptor"); } g_dbad = 1; }

/* the start of a descriptor statement: its descriptor by name */
void cob_sql_desc_begin(const char *lit)
{
    char name[132]; desc_name(lit, name);
    set_status(0, "00000"); g_rows = 0; g_dbad = 0; g_ditem = NULL;
    g_dcur = desc_find(name);
    if (!g_dcur) desc_fail(-1, "33000");            /* invalid SQL descriptor name */
}

void cob_sql_desc_alloc(const char *lit, int max)
{
    char name[132]; desc_name(lit, name);
    set_status(0, "00000"); g_rows = 0; g_dbad = 0;
    if (desc_find(name) || g_ndesc == 64) desc_fail(-1, "33000");
    else {
        desc_area *d = calloc(1, sizeof *d);
        snprintf(d->name, sizeof d->name, "%s", name);
        d->max = max > 0 ? max : 100;               /* the implementation's default maximum */
        d->it = calloc((size_t)d->max, sizeof *d->it);
        g_descs[g_ndesc++] = d;
    }
    diag_end("ALLOCATE DESCRIPTOR", NULL);
}

void cob_sql_desc_dealloc(const char *lit)
{
    cob_sql_desc_begin(lit);
    if (g_dcur) {
        for (int k = 0; k < g_dcur->max; k++) free(g_dcur->it[k].tv);
        free(g_dcur->it);
        for (int i = 0; i < g_ndesc; i++) if (g_descs[i] == g_dcur) g_descs[i] = g_descs[--g_ndesc];
        free(g_dcur); g_dcur = NULL;
    }
    diag_end("DEALLOCATE DESCRIPTOR", NULL);
}

/* VALUE n */
void cob_sql_desc_value_n(int n)
{
    if (!g_dcur) return;
    if (n < 1 || n > g_dcur->max) { desc_fail(-1, "07009"); g_ditem = NULL; return; }   /* invalid descriptor index */
    g_ditem = &g_dcur->it[n - 1];
}
void cob_sql_desc_value(void *p, const cob_desc *d) { cob_sql_desc_value_n((int)cob_get_num(p, d)); }

static void desc_set_value(desc_item *it, int vty, long long iv, const char *t, int n)
{
    free(it->tv); it->tv = NULL; it->tn = 0;
    it->vty = vty; it->iv = iv;
    if (t) { it->tv = malloc((size_t)n + 1); memcpy(it->tv, t, (size_t)n); it->tv[n] = 0; it->tn = n; }
}

/* SET DESCRIPTOR: a field from an integer */
void cob_sql_desc_set_n(int field, int v)
{
    if (!g_dcur) return;
    if (field == DF_COUNT) { if (v < 0 || v > g_dcur->max) desc_fail(-1, "07009"); else g_dcur->count = (int)v; return; }
    desc_item *it = g_ditem;
    if (!it) return;
    switch (field) {
    case DF_TYPE:
        /* a new type resets the item's other fields (ISO 17.5 SET DESCRIPTOR GR 4) */
        memset(it, 0, offsetof(desc_item, name)); it->type = (int)v;
        if (v == 1 || v == 12) it->length = 1;
        break;
    case DF_LENGTH: it->length = (int)v; break;
    case DF_OCTET_LENGTH: it->octet = (int)v; break;
    case DF_PRECISION: it->precision = (int)v; break;
    case DF_SCALE: it->scale = (int)v; break;
    case DF_DI_CODE: it->dicode = (int)v; break;
    case DF_DI_PRECISION: it->diprec = (int)v; break;
    case DF_NULLABLE: it->nullable = (int)v; break;
    case DF_INDICATOR: it->indicator = (int)v; break;
    case DF_UNNAMED: it->unnamed = (int)v; break;
    case DF_DATA: { char b[32]; int n = snprintf(b, sizeof b, "%lld", v); desc_set_value(it, SQLITE_INTEGER, v, b, n); break; }
    default: break;
    }
}

/* SET DESCRIPTOR: a field from a host variable (DATA takes its value as
 * the item's type says: text for CHARACTER and VARCHAR, a number else) */
void cob_sql_desc_set(void *p, const cob_desc *d, int field)
{
    if (!g_dcur) return;
    if (field == DF_NAME) {
        host h = { p, d, NULL, NULL }; char b[132]; host_text(&h, b, sizeof b);
        if (g_ditem) snprintf(g_ditem->name, sizeof g_ditem->name, "%s", b);
        return;
    }
    if (field != DF_DATA) { cob_sql_desc_set_n(field, cob_get_num(p, d)); return; }
    desc_item *it = g_ditem;
    if (!it) return;
    int chartype = it->type == 1 || it->type == 12;
    if (d->cat == COB_NUM && !chartype) {
        long long v = cob_get_num(p, d);
        if (d->scale <= 0) { for (int k = 0; k < -d->scale; k++) v *= 10; char b[32]; int n = snprintf(b, sizeof b, "%lld", v); desc_set_value(it, SQLITE_INTEGER, v, b, n); }
        else {
            unsigned long long m = v < 0 ? 0 - (unsigned long long)v : (unsigned long long)v;
            char b[48]; int n = snprintf(b, sizeof b, "%s%llu.%0*llu", v < 0 ? "-" : "", m / (unsigned long long)p10[d->scale], d->scale, m % (unsigned long long)p10[d->scale]);
            desc_set_value(it, SQLITE_TEXT, 0, b, n);
        }
    } else {
        char b[512]; host h = { p, d, NULL, NULL }; host_text(&h, b, sizeof b);
        if (chartype && it->type == 1 && it->length > 0 && (int)strlen(b) > it->length) b[it->length] = 0;
        desc_set_value(it, SQLITE_TEXT, 0, b, (int)strlen(b));
    }
}

/* GET DESCRIPTOR: a field into a host variable */
void cob_sql_desc_get(void *p, const cob_desc *d, int field)
{
    if (!g_dcur) return;
    long long v = 0; const char *text = NULL;
    desc_item *it = g_ditem;
    if (field != DF_COUNT && !it) return;
    switch (field) {
    case DF_COUNT: v = g_dcur->count; break;
    case DF_TYPE: v = it->type; break;
    case DF_LENGTH: v = it->length; break;
    case DF_OCTET_LENGTH: v = it->octet ? it->octet : it->length; break;
    case DF_RETURNED_LENGTH: case DF_RETURNED_OCTET_LENGTH: v = it->tn; break;
    case DF_PRECISION: v = it->precision; break;
    case DF_SCALE: v = it->scale; break;
    case DF_DI_CODE: v = it->dicode; break;
    case DF_DI_PRECISION: v = it->diprec; break;
    case DF_NULLABLE: v = it->nullable; break;
    case DF_INDICATOR: v = it->vty == SQLITE_NULL ? -1 : it->indicator; break;
    case DF_UNNAMED: v = it->unnamed; break;
    case DF_NAME: text = it->name; break;
    case DF_DATA: {
        host h = { p, d, NULL, NULL };
        if (it->vty == SQLITE_NULL || (!it->tv && it->vty != SQLITE_INTEGER)) return;   /* NULL: DATA is left alone (the INDICATOR says) */
        if (!store_val(it->vty ? it->vty : SQLITE_TEXT, it->iv, it->tv, it->tn, &h)) g_dbad = 1;
        return;
    }
    default: text = ""; break;
    }
    if (text) {
        cob_desc sd; memset(&sd, 0, sizeof sd); sd.cat = COB_ALNUM; sd.size = (unsigned)strlen(text);
        if (sd.size) cob_move(text, &sd, p, d); else memset(p, ' ', d->size);
    } else if (d->cat == COB_NUM) cob_put_num(p, d, v, 0);
}

/* the end of a GET or SET DESCRIPTOR statement */
void cob_sql_desc_end(int get) { g_dcur = NULL; g_ditem = NULL; diag_end(get ? "GET DESCRIPTOR" : "SET DESCRIPTOR", NULL); }

/* the declared type of a column as the descriptor's TYPE, LENGTH,
 * PRECISION and SCALE (ISO codes: 1 CHARACTER, 2 NUMERIC, 3 DECIMAL,
 * 4 INTEGER, 5 SMALLINT, 6 FLOAT, 7 REAL, 8 DOUBLE PRECISION, 12 VARCHAR);
 * an expression SQLite gives no declared type: NUMERIC (a behavior point) */
static void desc_from_decl(desc_item *it, const char *decl)
{
    int a = 0, b = 0, na = 0;
    const char *lp = decl ? strchr(decl, '(') : NULL;
    if (lp) { na = sscanf(lp, "(%d , %d", &a, &b); if (na < 1) na = sscanf(lp, "(%d,%d", &a, &b); }
    char up[64] = ""; int k = 0;
    for (const char *q = decl ? decl : ""; *q && *q != '(' && k < 63; q++) up[k++] = (char)toupper((unsigned char)*q);
    while (k && up[k - 1] == ' ') k--; up[k] = 0;
    it->nullable = 1;
    if (!decl || !*decl) { it->type = 2; it->unnamed = 1; return; }
    if (strstr(up, "VARYING") || !strncmp(up, "VARCHAR", 7)) { it->type = 12; it->length = na >= 1 ? a : 1; }
    else if (!strncmp(up, "CHAR", 4) || !strncmp(up, "NCHAR", 5) || !strncmp(up, "NATIONAL", 8)) { it->type = 1; it->length = na >= 1 ? a : 1; }
    else if (!strncmp(up, "NUMERIC", 7)) { it->type = 2; it->precision = na >= 1 ? a : 18; it->scale = na >= 2 ? b : 0; }
    else if (!strncmp(up, "DEC", 3)) { it->type = 3; it->precision = na >= 1 ? a : 18; it->scale = na >= 2 ? b : 0; }
    else if (!strncmp(up, "INT", 3)) { it->type = 4; it->precision = 10; }
    else if (!strncmp(up, "SMALLINT", 8)) { it->type = 5; it->precision = 5; }
    else if (!strncmp(up, "FLOAT", 5)) { it->type = 6; it->precision = na >= 1 ? a : 53; }
    else if (!strncmp(up, "REAL", 4)) { it->type = 7; it->precision = 24; }
    else if (!strncmp(up, "DOUBLE", 6)) { it->type = 8; it->precision = 53; }
    else it->type = 1;
    it->octet = it->length;
}

/* DESCRIBE [INPUT | OUTPUT] s USING SQL DESCRIPTOR d */
void cob_sql_describe(cob_sql_dyn *s, const char *lit, int input)
{
    cob_sql_desc_begin(lit);
    if (g_dcur) {
        if (!s->st) desc_fail(-518, "26000");
        else if (input) {
            int n = db_bind_parameter_count(s->st);
            g_dcur->count = n;
            if (n > g_dcur->max) { set_warning("01005"); n = g_dcur->max; }   /* insufficient item descriptor areas */
            for (int k = 0; k < n; k++) { desc_item *it = &g_dcur->it[k]; memset(it, 0, offsetof(desc_item, vty)); it->type = 1; it->length = 1; it->nullable = 1; it->unnamed = 1; }
        } else {
            int n = db_column_count(s->st), base = s->rowid ? 1 : 0;
            n -= base;
            g_dcur->count = n;
            if (n > g_dcur->max) { set_warning("01005"); n = g_dcur->max; }
            for (int k = 0; k < n; k++) {
                desc_item *it = &g_dcur->it[k];
                memset(it, 0, offsetof(desc_item, vty));
                desc_from_decl(it, db_column_decltype(s->st, base + k));
                const char *nm = db_column_name(s->st, base + k);
                snprintf(it->name, sizeof it->name, "%s", nm ? nm : "");
            }
        }
    }
    g_dcur = NULL;
    diag_end("DESCRIBE", NULL);
}

/* USING / INTO SQL DESCRIPTOR: the next statement's */
void cob_sql_desc_using(const char *lit) { char n[132]; desc_name(lit, n); g_desc_in = desc_find(n); if (!g_desc_in) g_desc_in = (desc_area *)-1; }
void cob_sql_desc_into(const char *lit) { char n[132]; desc_name(lit, n); g_desc_out = desc_find(n); if (!g_desc_out) g_desc_out = (desc_area *)-1; }

/* binding a statement's parameters from the USING descriptor: 1 done, 0 failed */
static int bind_desc(dbst *st)
{
    desc_area *d = g_desc_in; g_desc_in = NULL;
    if (d == (desc_area *)-1) { set_status(-1, "33000"); return 0; }
    for (int k = 0; k < d->count; k++) {
        desc_item *it = &d->it[k];
        if (it->indicator < 0 || it->vty == SQLITE_NULL || (it->vty == 0 && !it->tv)) db_bind_null(st, k + 1);
        else if (it->vty == SQLITE_INTEGER) db_bind_int64(st, k + 1, it->iv);
        else db_bind_text(st, k + 1, it->tv, it->tn);
    }
    return 1;
}

/* a row into the INTO descriptor: its items' types and names as DESCRIBE
 * would set them, their values, the NULLs as indicators */
static int store_desc(dbst *st, int base)
{
    desc_area *d = g_desc_out; g_desc_out = NULL;
    if (d == (desc_area *)-1) { set_status(-1, "33000"); return 0; }
    int n = db_column_count(st) - base;
    if (n > d->max) { set_status(-1, "07008"); return 0; }
    d->count = n;
    for (int k = 0; k < n; k++) {
        desc_item *it = &d->it[k];
        int ty = db_column_type(st, base + k);
        if (!it->type) desc_from_decl(it, db_column_decltype(st, base + k));
        const char *t = ty == SQLITE_NULL ? NULL : (const char *)db_column_text(st, base + k);
        desc_set_value(it, ty, ty == SQLITE_INTEGER ? db_column_int64(st, base + k) : 0, t, t ? db_column_bytes(st, base + k) : 0);
        it->indicator = ty == SQLITE_NULL ? -1 : 0;
    }
    return 1;
}

/* ---- the program's status items ---------------------------------------- */

/* for WHENEVER: the last statement's SQLCODE, and whether it warned */
int cob_sql_code(void) { return g_sqlcode; }
int cob_sql_warn(void) { return !memcmp(g_sqlstate, "01", 2); }

void cob_sql_put_sqlcode(void *p, const cob_desc *d)
{
    if (d->cat == COB_NUM) cob_put_num(p, d, g_sqlcode, 0);
}

/* an SQLCA field: 0 SQLERRML, 1 SQLERRMC, 2 SQLERRD(3), 3 SQLWARN0, 4 SQLWARN1 */
void cob_sql_put_field(void *p, const cob_desc *d, int which)
{
    cob_desc sd; memset(&sd, 0, sizeof sd); sd.cat = COB_ALNUM;
    int warn = !memcmp(g_sqlstate, "01", 2);
    switch (which) {
    case 0: if (d->cat == COB_NUM) cob_put_num(p, d, (long long)strlen(g_errmsg), 0); break;
    case 1: sd.size = (unsigned)strlen(g_errmsg); if (sd.size) cob_move(g_errmsg, &sd, p, d); else memset(p, ' ', d->size); break;
    case 2: if (d->cat == COB_NUM) cob_put_num(p, d, g_rows, 0); break;
    case 3: sd.size = 1; cob_move(warn ? "W" : " ", &sd, p, d); break;
    case 4: sd.size = 1; cob_move(!memcmp(g_sqlstate, "01004", 5) ? "W" : " ", &sd, p, d); break;
    }
}

void cob_sql_put_sqlstate(void *p, const cob_desc *d)
{
    cob_desc sd; memset(&sd, 0, sizeof sd);
    sd.cat = COB_ALNUM; sd.size = 5;
    cob_move(g_sqlstate, &sd, p, d);
}

/* the PostgreSQL backend: the client and its SCRAM, built into this object
 * so a program with SQL links one runtime object, as before */
#include "scram.c"
#include "pgwire.c"
