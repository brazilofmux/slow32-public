/* pgwire.c -- see pgwire.h.  The protocol is PostgreSQL's "Frontend/Backend
 * Protocol", version 3.0: messages of a type byte and a 32-bit big-endian
 * length that counts itself.  Statements go through the extended query
 * protocol -- Parse and Describe when prepared, Bind and Execute when
 * first stepped -- with every parameter and every result column as text,
 * so the runtime converts values exactly as it does SQLite's text. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "pgwire.h"
#include "scram.h"

struct pg_conn {
    int fd;
    unsigned char *in; int in_len, in_pos, in_cap;    /* received, not yet read */
    unsigned char *out; int out_len, out_cap;         /* the message being built */
    char err[512];
    char state[6];
    char txn;                                         /* ReadyForQuery: I, T or E */
    long long changes;
    unsigned next_stmt;
};

struct pg_stmt {
    pg_conn *c;
    char name[16];
    int nparams;
    char **pval; int *plen;                           /* bound values, NULL for SQL NULL */
    int ncols;
    char **colname; unsigned *coltype;
    int executed;                                     /* Bind/Execute sent and read */
    /* the result: every row's values in data, each a length (-1 NULL) and
     * an offset */
    char *data; long long data_len, data_cap;
    int *vlen; long long *voff; long long nvals, vcap;
    long long nrows, cur;                             /* cur: the row step returned last, -1 none */
};

/* ---- errors -------------------------------------------------------------- */

static int fail(pg_conn *c, const char *state, const char *msg)
{
    snprintf(c->err, sizeof c->err, "%s", msg);
    memcpy(c->state, state, 5); c->state[5] = 0;
    return PG_ERROR;
}

const char *pg_errmsg(pg_conn *c) { return c ? c->err : "no connection"; }
const char *pg_sqlstate(pg_conn *c) { return c ? c->state : "08003"; }

/* ---- the socket ---------------------------------------------------------- */

static int read_more(pg_conn *c)
{
    if (c->in_pos > 0 && c->in_pos == c->in_len) c->in_pos = c->in_len = 0;
    if (c->in_len == c->in_cap) {
        if (c->in_pos > 0) {
            memmove(c->in, c->in + c->in_pos, (size_t)(c->in_len - c->in_pos));
            c->in_len -= c->in_pos; c->in_pos = 0;
        } else {
            int cap = c->in_cap ? c->in_cap * 2 : 65536;
            unsigned char *p = realloc(c->in, (size_t)cap);
            if (!p) return fail(c, "53200", "out of memory");
            c->in = p; c->in_cap = cap;
        }
    }
    int n = (int)recv(c->fd, c->in + c->in_len, c->in_cap - c->in_len, 0);
    if (n <= 0) return fail(c, "08006", "the server closed the connection");
    c->in_len += n;
    return PG_OK;
}

static unsigned get32(const unsigned char *p) { return (unsigned)p[0] << 24 | (unsigned)p[1] << 16 | (unsigned)p[2] << 8 | p[3]; }
static unsigned get16(const unsigned char *p) { return (unsigned)p[0] << 8 | p[1]; }

/* the next message: its type, its body and the body's length.  The body
 * stays valid until the next call. */
static int next_msg(pg_conn *c, char *type, const unsigned char **body, int *len)
{
    while (c->in_len - c->in_pos < 5)
        if (read_more(c)) return PG_ERROR;
    unsigned n = get32(c->in + c->in_pos + 1);
    if (n < 4 || n > (1u << 30)) return fail(c, "08P01", "a malformed message from the server");
    while ((unsigned)(c->in_len - c->in_pos) < n + 1)
        if (read_more(c)) return PG_ERROR;
    *type = (char)c->in[c->in_pos];
    *body = c->in + c->in_pos + 5;
    *len = (int)n - 4;
    c->in_pos += (int)n + 1;
    return PG_OK;
}

static int out_reserve(pg_conn *c, int n)
{
    if (c->out_len + n <= c->out_cap) return PG_OK;
    int cap = c->out_cap ? c->out_cap : 4096;
    while (c->out_len + n > cap) cap *= 2;
    unsigned char *p = realloc(c->out, (size_t)cap);
    if (!p) return fail(c, "53200", "out of memory");
    c->out = p; c->out_cap = cap;
    return PG_OK;
}
static void put_bytes(pg_conn *c, const void *p, int n) { if (!out_reserve(c, n)) { memcpy(c->out + c->out_len, p, (size_t)n); c->out_len += n; } }
static void put8(pg_conn *c, int v) { unsigned char b = (unsigned char)v; put_bytes(c, &b, 1); }
static void put16(pg_conn *c, int v) { unsigned char b[2] = { (unsigned char)(v >> 8), (unsigned char)v }; put_bytes(c, b, 2); }
static void put32(pg_conn *c, int v) { unsigned char b[4] = { (unsigned char)(v >> 24), (unsigned char)(v >> 16), (unsigned char)(v >> 8), (unsigned char)v }; put_bytes(c, b, 4); }
static void put_str(pg_conn *c, const char *s) { put_bytes(c, s, (int)strlen(s) + 1); }

/* a message of type t (0: the untyped startup message), its length filled
 * in by msg_end */
static int g_msg_start;
static void msg_begin(pg_conn *c, char t) { if (t) put8(c, t); g_msg_start = c->out_len; put32(c, 0); }
static void msg_end(pg_conn *c)
{
    int n = c->out_len - g_msg_start;
    c->out[g_msg_start] = (unsigned char)(n >> 24); c->out[g_msg_start + 1] = (unsigned char)(n >> 16);
    c->out[g_msg_start + 2] = (unsigned char)(n >> 8); c->out[g_msg_start + 3] = (unsigned char)n;
}

static int flush_out(pg_conn *c)
{
    int off = 0;
    while (off < c->out_len) {
        int n = (int)send(c->fd, c->out + off, c->out_len - off, 0);
        if (n <= 0) { c->out_len = 0; return fail(c, "08006", "the connection was lost while sending"); }
        off += n;
    }
    c->out_len = 0;
    return PG_OK;
}

/* ErrorResponse: the SQLSTATE (C) and message (M) fields */
static int server_error(pg_conn *c, const unsigned char *b, int len)
{
    const char *state = "XX000", *msg = "an error from the server";
    for (int i = 0; i < len && b[i]; ) {
        char code = (char)b[i++];
        const char *v = (const char *)b + i;
        if (code == 'C') state = v;
        else if (code == 'M') msg = v;
        i += (int)strlen(v) + 1;
    }
    return fail(c, strlen(state) == 5 ? state : "XX000", msg);
}

/* read to ReadyForQuery, keeping the first error; the messages a
 * statement's reply may carry and this caller does not want are skipped */
static int sync_reply(pg_conn *c, int rc)
{
    for (;;) {
        char t; const unsigned char *b; int n;
        if (next_msg(c, &t, &b, &n)) return PG_ERROR;
        if (t == 'Z') { c->txn = n > 0 ? (char)b[0] : 'I'; return rc; }
        if (t == 'E' && rc == PG_OK) rc = server_error(c, b, n);
        if (t == 'C') {                                 /* CommandComplete: "INSERT 0 5", "UPDATE 3" */
            const char *sp = strrchr((const char *)b, ' ');
            c->changes = sp ? atoll(sp + 1) : 0;
        }
    }
}

/* ---- the connection ------------------------------------------------------ */

static void make_nonce(char *out, int outsz)
{
    unsigned char r[18];
    FILE *f = fopen("/dev/urandom", "rb");
    size_t got = f ? fread(r, 1, sizeof r, f) : 0;
    if (f) fclose(f);
    if (got != sizeof r) {                              /* no urandom: the time and an address */
        unsigned long long x = (unsigned long long)time(NULL) * 6364136223846793005ULL ^ (unsigned long long)(size_t)&r;
        for (int i = 0; i < (int)sizeof r; i++) { x = x * 6364136223846793005ULL + 1442695040888963407ULL; r[i] = (unsigned char)(x >> 56); }
    }
    b64_encode(r, sizeof r, out, outsz);
}

static int authenticate(pg_conn *c, const char *user, const char *password)
{
    scram_state sc;
    for (;;) {
        char t; const unsigned char *b; int n;
        if (next_msg(c, &t, &b, &n)) return PG_ERROR;
        if (t == 'E') return server_error(c, b, n);
        if (t != 'R' || n < 4) return fail(c, "08P01", "expected an authentication request");
        unsigned code = get32(b);
        if (code == 0) return PG_OK;                    /* AuthenticationOk */
        if (code == 3) {                                /* cleartext password */
            if (!password) return fail(c, "28P01", "the server asks for a password and none was given (PGPASSWORD)");
            msg_begin(c, 'p'); put_str(c, password); msg_end(c);
            if (flush_out(c)) return PG_ERROR;
            continue;
        }
        if (code == 5) return fail(c, "28000", "MD5 password authentication is not implemented; SCRAM-SHA-256 is");
        if (code == 10) {                               /* AuthenticationSASL: the mechanisms */
            int ok = 0;
            for (int i = 4; i < n && b[i]; i += (int)strlen((const char *)b + i) + 1)
                if (!strcmp((const char *)b + i, "SCRAM-SHA-256")) ok = 1;
            if (!ok) return fail(c, "28000", "the server offers no SASL mechanism this client has (SCRAM-SHA-256)");
            if (!password) return fail(c, "28P01", "the server asks for a password and none was given (PGPASSWORD)");
            char nonce[32], first[300];
            make_nonce(nonce, sizeof nonce);
            /* the user name in the SCRAM message is ignored by PostgreSQL;
             * it uses the startup message's, so send none */
            int fl = scram_client_first(&sc, "", nonce, first, sizeof first);
            if (fl < 0) return fail(c, "08P01", "SCRAM: the first message would not build");
            msg_begin(c, 'p'); put_str(c, "SCRAM-SHA-256"); put32(c, fl); put_bytes(c, first, fl); msg_end(c);
            if (flush_out(c)) return PG_ERROR;
            continue;
        }
        if (code == 11) {                               /* AuthenticationSASLContinue: server-first */
            char final[512];
            int fl = scram_client_final(&sc, password, (const char *)b + 4, n - 4, final, sizeof final);
            if (fl < 0) return fail(c, "08P01", "SCRAM: the server's first message is malformed, or its nonce is not ours");
            msg_begin(c, 'p'); put_bytes(c, final, fl); msg_end(c);
            if (flush_out(c)) return PG_ERROR;
            continue;
        }
        if (code == 12) {                               /* AuthenticationSASLFinal: the server's proof */
            if (!scram_verify_server(&sc, (const char *)b + 4, n - 4))
                return fail(c, "28000", "SCRAM: the server's signature is wrong -- not the server it claims to be");
            continue;
        }
        char msg[96];
        snprintf(msg, sizeof msg, "authentication method %u is not implemented", code);
        return fail(c, "28000", msg);
    }
}

int pg_connect(const char *host, int port, const char *user, const char *db,
               const char *password, pg_conn **out, char *err, int errsz)
{
    *out = NULL;
    pg_conn *c = calloc(1, sizeof *c);
    if (!c) { snprintf(err, (size_t)errsz, "out of memory"); return PG_ERROR; }
    c->fd = -1; c->txn = 'I';
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET; a.sin_port = htons((unsigned short)port);
    if (!inet_aton(host, &a.sin_addr)) {
        snprintf(err, (size_t)errsz, "PGHOST %s is not an IPv4 address (the guest has no DNS)", host);
        free(c); return PG_ERROR;
    }
    c->fd = socket(AF_INET, SOCK_STREAM, 0);
    if (c->fd < 0 || connect(c->fd, (struct sockaddr *)&a, sizeof a) < 0) {
        snprintf(err, (size_t)errsz, "could not connect to %s:%d", host, port);
        if (c->fd >= 0) close(c->fd);
        free(c); return PG_ERROR;
    }
    msg_begin(c, 0);
    put32(c, 196608);                                   /* protocol 3.0 */
    put_str(c, "user"); put_str(c, user);
    put_str(c, "database"); put_str(c, db);
    put_str(c, "client_encoding"); put_str(c, "UTF8");
    put8(c, 0);
    msg_end(c);
    int rc = flush_out(c);
    if (!rc) rc = authenticate(c, user, password);
    if (!rc) rc = sync_reply(c, PG_OK);                 /* ParameterStatus, BackendKeyData, ReadyForQuery */
    if (rc) { snprintf(err, (size_t)errsz, "%s", c->err); pg_close(c); return PG_ERROR; }
    *out = c;
    return PG_OK;
}

void pg_close(pg_conn *c)
{
    if (!c) return;
    if (c->fd >= 0) {
        msg_begin(c, 'X'); msg_end(c);                  /* Terminate */
        flush_out(c);
        close(c->fd);
    }
    free(c->in); free(c->out); free(c);
}

int pg_exec(pg_conn *c, const char *sql)
{
    msg_begin(c, 'Q'); put_str(c, sql); msg_end(c);
    if (flush_out(c)) return PG_ERROR;
    c->err[0] = 0; memcpy(c->state, "00000", 6);
    return sync_reply(c, PG_OK);
}

int pg_in_transaction(pg_conn *c) { return c && c->txn != 'I'; }
long long pg_changes(pg_conn *c) { return c ? c->changes : 0; }

/* ---- the password file (libpq's .pgpass) --------------------------------- */

/* libpq's pwdfMatchesString: the field at buf matches token ('*' anything);
 * the text after its ':', or NULL */
static const char *pgpass_field(const char *buf, const char *token)
{
    if (buf[0] == '*' && buf[1] == ':') return buf + 2;
    const char *t = buf, *k = token;
    int bslash = 0;
    while (*t) {
        if (*t == '\\' && !bslash) { t++; bslash = 1; }
        if (*t == ':' && !*k && !bslash) return t + 1;
        bslash = 0;
        if (!*k || *t != *k) return NULL;
        t++; k++;
    }
    return NULL;
}

int pg_password_from_file(const char *path, const char *host, const char *port, const char *db,
                          const char *user, char *out, int outsz)
{
    struct stat st;
    if (stat(path, &st) || !S_ISREG(st.st_mode) || (st.st_mode & 0077)) return 0;
    FILE *f = fopen(path, "r");
    if (!f) return 0;
    char line[1024];
    int found = 0;
    while (!found && fgets(line, sizeof line, f)) {
        size_t n = strlen(line);
        while (n && (line[n - 1] == '\n' || line[n - 1] == '\r')) line[--n] = 0;
        if (!n || line[0] == '#') continue;
        const char *t = line;
        if (!(t = pgpass_field(t, host)) || !(t = pgpass_field(t, port)) ||
            !(t = pgpass_field(t, db)) || !(t = pgpass_field(t, user))) continue;
        /* the password: to the first unescaped ':', escapes removed */
        int k = 0;
        for (const char *p = t; *p && *p != ':' && k < outsz - 1; p++) {
            if (*p == '\\' && p[1]) p++;
            out[k++] = *p;
        }
        out[k] = 0;
        found = 1;
    }
    fclose(f);
    return found;
}

/* ---- statements ---------------------------------------------------------- */

/* ? to $n, outside '...' literals (with '' inside), "..." names and
 * -- comments */
static char *dollar_params(const char *sql, int *nparams)
{
    size_t n = strlen(sql);
    char *out = malloc(n * 4 + 1);
    int k = 0, p = 0;
    if (!out) return NULL;
    for (size_t i = 0; i < n; ) {
        char ch = sql[i];
        if (ch == '\'' || ch == '"') {
            out[k++] = sql[i++];
            while (i < n) {
                out[k++] = sql[i];
                if (sql[i++] == ch) { if (i < n && sql[i] == ch) { out[k++] = sql[i++]; continue; } break; }
            }
        } else if (ch == '-' && i + 1 < n && sql[i + 1] == '-') {
            while (i < n && sql[i] != '\n') out[k++] = sql[i++];
        } else if (ch == '?') {
            k += sprintf(out + k, "$%d", ++p); i++;
        } else out[k++] = sql[i++];
    }
    out[k] = 0;
    *nparams = p;
    return out;
}

int pg_prepare(pg_conn *c, const char *sql, pg_stmt **out)
{
    *out = NULL;
    int np;
    char *text = dollar_params(sql, &np);
    pg_stmt *s = calloc(1, sizeof *s);
    if (!text || !s) { free(text); free(s); return fail(c, "53200", "out of memory"); }
    s->c = c; s->cur = -1;
    snprintf(s->name, sizeof s->name, "s32_%u", ++c->next_stmt);
    msg_begin(c, 'P'); put_str(c, s->name); put_str(c, text); put16(c, 0); msg_end(c);   /* Parse: the server infers the types */
    msg_begin(c, 'D'); put8(c, 'S'); put_str(c, s->name); msg_end(c);                    /* Describe the statement */
    msg_begin(c, 'S'); msg_end(c);                                                      /* Sync */
    free(text);
    c->err[0] = 0; memcpy(c->state, "00000", 6);
    if (flush_out(c)) { free(s); return PG_ERROR; }
    int rc = PG_OK;
    for (;;) {
        char t; const unsigned char *b; int n;
        if (next_msg(c, &t, &b, &n)) { pg_finalize(s); return PG_ERROR; }
        if (t == 'Z') { c->txn = n > 0 ? (char)b[0] : 'I'; break; }
        if (t == 'E' && rc == PG_OK) rc = server_error(c, b, n);
        if (t == 't' && n >= 2) s->nparams = (int)get16(b);              /* ParameterDescription */
        if (t == 'T' && n >= 2) {                                         /* RowDescription */
            int nc = (int)get16(b), off = 2;
            s->colname = calloc((size_t)(nc ? nc : 1), sizeof *s->colname);
            s->coltype = calloc((size_t)(nc ? nc : 1), sizeof *s->coltype);
            for (int i = 0; i < nc && off < n; i++) {
                s->colname[i] = strdup((const char *)b + off);
                off += (int)strlen((const char *)b + off) + 1;
                if (off + 18 <= n) s->coltype[i] = get32(b + off + 6);
                off += 18;
            }
            s->ncols = nc;
        }
    }
    if (rc) { pg_finalize(s); return PG_ERROR; }
    if (s->nparams < np) s->nparams = np;
    s->pval = calloc((size_t)(s->nparams ? s->nparams : 1), sizeof *s->pval);
    s->plen = calloc((size_t)(s->nparams ? s->nparams : 1), sizeof *s->plen);
    *out = s;
    return PG_OK;
}

int pg_param_count(pg_stmt *s) { return s->nparams; }
int pg_column_count(pg_stmt *s) { return s->ncols; }
const char *pg_column_name(pg_stmt *s, int i) { return i >= 0 && i < s->ncols ? s->colname[i] : ""; }
unsigned pg_column_type(pg_stmt *s, int i) { return i >= 0 && i < s->ncols ? s->coltype[i] : 0; }

int pg_bind_text(pg_stmt *s, int i, const char *text, int len)
{
    if (i < 1 || i > s->nparams) return fail(s->c, "07001", "a parameter number out of range");
    free(s->pval[i - 1]); s->pval[i - 1] = NULL;
    if (text) {
        if (len < 0) len = (int)strlen(text);
        s->pval[i - 1] = malloc((size_t)len + 1);
        if (!s->pval[i - 1]) return fail(s->c, "53200", "out of memory");
        memcpy(s->pval[i - 1], text, (size_t)len); s->pval[i - 1][len] = 0;
        s->plen[i - 1] = len;
    }
    return PG_OK;
}

void pg_clear_bindings(pg_stmt *s)
{
    for (int i = 0; i < s->nparams; i++) { free(s->pval[i]); s->pval[i] = NULL; }
}

static int keep_value(pg_stmt *s, const unsigned char *p, int len)
{
    if (s->nvals == s->vcap) {
        long long cap = s->vcap ? s->vcap * 2 : 1024;
        int *vl = realloc(s->vlen, (size_t)cap * sizeof *vl);
        if (!vl) return fail(s->c, "53200", "out of memory");
        s->vlen = vl;
        long long *vo = realloc(s->voff, (size_t)cap * sizeof *vo);
        if (!vo) return fail(s->c, "53200", "out of memory");
        s->voff = vo; s->vcap = cap;
    }
    long long need = s->data_len + (len > 0 ? len : 0) + 1;    /* the value, NUL-terminated */
    if (need > s->data_cap) {
        long long cap = s->data_cap ? s->data_cap : 65536;
        while (need > cap) cap *= 2;
        char *d = realloc(s->data, (size_t)cap);
        if (!d) return fail(s->c, "53200", "out of memory");
        s->data = d; s->data_cap = cap;
    }
    s->vlen[s->nvals] = len;
    s->voff[s->nvals] = s->data_len;
    if (len > 0) { memcpy(s->data + s->data_len, p, (size_t)len); s->data_len += len; }
    s->data[s->data_len++] = 0;
    s->nvals++;
    return PG_OK;
}

/* Bind and Execute, the rows read into the statement */
static int execute(pg_stmt *s)
{
    pg_conn *c = s->c;
    msg_begin(c, 'B');
    put_str(c, "");                             /* the unnamed portal */
    put_str(c, s->name);
    put16(c, 0);                                /* every parameter as text */
    put16(c, s->nparams);
    for (int i = 0; i < s->nparams; i++) {
        if (!s->pval[i]) put32(c, -1);
        else { put32(c, s->plen[i]); put_bytes(c, s->pval[i], s->plen[i]); }
    }
    put16(c, 0);                                /* every column as text */
    msg_end(c);
    msg_begin(c, 'E'); put_str(c, ""); put32(c, 0); msg_end(c);     /* Execute, every row */
    msg_begin(c, 'S'); msg_end(c);
    c->err[0] = 0; memcpy(c->state, "00000", 6);
    c->changes = 0;
    if (flush_out(c)) return PG_ERROR;
    s->nvals = 0; s->data_len = 0; s->nrows = 0; s->cur = -1;
    int rc = PG_OK;
    for (;;) {
        char t; const unsigned char *b; int n;
        if (next_msg(c, &t, &b, &n)) return PG_ERROR;
        if (t == 'Z') { c->txn = n > 0 ? (char)b[0] : 'I'; break; }
        if (t == 'E' && rc == PG_OK) rc = server_error(c, b, n);
        if (t == 'C') { const char *sp = strrchr((const char *)b, ' '); c->changes = sp ? atoll(sp + 1) : 0; }
        if (t == 'D' && rc == PG_OK && n >= 2) {                         /* DataRow */
            int nc = (int)get16(b), off = 2;
            for (int i = 0; i < nc; i++) {
                if (off + 4 > n) { rc = fail(c, "08P01", "a malformed row from the server"); break; }
                int len = (int)get32(b + off); off += 4;
                if (len >= 0 && off + len > n) { rc = fail(c, "08P01", "a malformed row from the server"); break; }
                if (keep_value(s, b + off, len) != PG_OK) { rc = PG_ERROR; break; }
                if (len > 0) off += len;
            }
            if (rc == PG_OK) s->nrows++;
        }
    }
    s->executed = 1;
    return rc;
}

int pg_step(pg_stmt *s)
{
    if (!s->executed && execute(s)) return PG_ERROR;
    if (s->cur + 1 >= s->nrows) { s->cur = s->nrows; return PG_DONE; }
    s->cur++;
    return PG_ROW;
}

int pg_reset(pg_stmt *s) { s->executed = 0; s->cur = -1; s->nrows = 0; return PG_OK; }

const char *pg_column_text(pg_stmt *s, int i, int *len)
{
    if (s->cur < 0 || s->cur >= s->nrows || i < 0 || i >= s->ncols) { if (len) *len = 0; return NULL; }
    long long v = s->cur * s->ncols + i;
    if (s->vlen[v] < 0) { if (len) *len = 0; return NULL; }
    if (len) *len = s->vlen[v];
    return s->data + s->voff[v];
}

void pg_finalize(pg_stmt *s)
{
    if (!s) return;
    pg_conn *c = s->c;
    if (c && c->fd >= 0 && s->name[0]) {
        msg_begin(c, 'C'); put8(c, 'S'); put_str(c, s->name); msg_end(c);   /* Close the statement */
        msg_begin(c, 'S'); msg_end(c);
        if (!flush_out(c)) sync_reply(c, PG_OK);
    }
    pg_clear_bindings(s);
    for (int i = 0; i < s->ncols; i++) free(s->colname ? s->colname[i] : NULL);
    free(s->colname); free(s->coltype); free(s->pval); free(s->plen);
    free(s->data); free(s->vlen); free(s->voff);
    free(s);
}
