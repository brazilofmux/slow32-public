/* pgwire.h -- a PostgreSQL client for the ESQL runtime (docs/esql.md):
 * the frontend/backend protocol 3.0 over a TCP socket, SCRAM-SHA-256 or
 * cleartext or no authentication, and statements in the shape the runtime
 * drives SQLite through -- prepare, bind, step, column, reset, finalize --
 * with every value as text.  Plain POSIX C: the same file is the SLOW-32
 * guest's (the MMIO sockets, runtime/net_mmio.c) and the host's, where
 * tests/pgwire_test.c drives it against a server. */
#ifndef S32_PGWIRE_H
#define S32_PGWIRE_H

typedef struct pg_conn pg_conn;
typedef struct pg_stmt pg_stmt;

enum { PG_OK = 0, PG_ROW = 100, PG_DONE = 101, PG_ERROR = 1 };

/* host: an IPv4 address (the guest has no DNS); 0 on success.  On failure
 * *out is NULL and err says why. */
int pg_connect(const char *host, int port, const char *user, const char *db,
               const char *password, pg_conn **out, char *err, int errsz);
void pg_close(pg_conn *c);

/* a statement with no parameters and no rows wanted (BEGIN, COMMIT, SET) */
int pg_exec(pg_conn *c, const char *sql);
/* 1 inside a transaction block, 0 idle (ReadyForQuery's status) */
int pg_in_transaction(pg_conn *c);

/* sql's parameters are ?, outside literals and quoted names, as the
 * compiler writes them; they become $1, $2, ... */
int pg_prepare(pg_conn *c, const char *sql, pg_stmt **out);
int pg_param_count(pg_stmt *s);
/* i from 1; text NULL is SQL NULL */
int pg_bind_text(pg_stmt *s, int i, const char *text, int len);
void pg_clear_bindings(pg_stmt *s);
int pg_step(pg_stmt *s);                    /* PG_ROW, PG_DONE or PG_ERROR */
int pg_reset(pg_stmt *s);
void pg_finalize(pg_stmt *s);
int pg_column_count(pg_stmt *s);
const char *pg_column_name(pg_stmt *s, int i);   /* i from 0 */
unsigned pg_column_type(pg_stmt *s, int i);      /* the type's OID */
/* the current row's value as text, NULL for SQL NULL; its length in *len */
const char *pg_column_text(pg_stmt *s, int i, int *len);
long long pg_changes(pg_conn *c);           /* the rows the last command touched */

/* the last error: the server's message and its SQLSTATE ("08001" and the
 * like for the connection's own) */
const char *pg_errmsg(pg_conn *c);
const char *pg_sqlstate(pg_conn *c);

#endif
