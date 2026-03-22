/*
 * main.c - Minimal SQLite3 shell for SLOW-32
 *
 * Creates an in-memory or file-backed database, runs a few SQL statements.
 */

#include <stdio.h>
#include <string.h>
#include "sqlite3.h"

/* VFS registered via sqlite3_os_init() during sqlite3_initialize() */

static int callback(void *data, int argc, char **argv, char **azColName) {
    (void)data;
    for (int i = 0; i < argc; i++) {
        if (i > 0) printf("|");
        printf("%s", argv[i] ? argv[i] : "NULL");
    }
    printf("\n");
    return 0;
}

int main(void) {
    sqlite3 *db;
    char *errmsg = 0;
    int rc;

    /* Initialize SQLite (calls sqlite3_os_init -> registers our VFS) */
    sqlite3_initialize();

    printf("SQLite version: %s\n", sqlite3_libversion());

    /* Open in-memory database */
    rc = sqlite3_open(":memory:", &db);
    if (rc) {
        printf("Can't open database: %s\n", sqlite3_errmsg(db));
        return 1;
    }
    printf("Database opened.\n");

    /* Create table */
    rc = sqlite3_exec(db,
        "CREATE TABLE test (id INTEGER PRIMARY KEY, name TEXT, value REAL);",
        0, 0, &errmsg);
    if (rc != SQLITE_OK) {
        printf("SQL error: %s\n", errmsg);
        sqlite3_free(errmsg);
        return 1;
    }
    printf("Table created.\n");

    /* Insert rows */
    sqlite3_exec(db, "INSERT INTO test VALUES(1, 'alpha', 1.5);", 0, 0, 0);
    sqlite3_exec(db, "INSERT INTO test VALUES(2, 'beta', 2.7);", 0, 0, 0);
    sqlite3_exec(db, "INSERT INTO test VALUES(3, 'gamma', 3.14);", 0, 0, 0);
    sqlite3_exec(db, "INSERT INTO test VALUES(4, 'delta', 42.0);", 0, 0, 0);
    sqlite3_exec(db, "INSERT INTO test VALUES(5, 'epsilon', 99.9);", 0, 0, 0);
    printf("5 rows inserted.\n\n");

    /* Query */
    printf("SELECT * FROM test:\n");
    sqlite3_exec(db, "SELECT * FROM test;", callback, 0, 0);

    printf("\nSELECT WHERE value > 3:\n");
    sqlite3_exec(db, "SELECT name, value FROM test WHERE value > 3.0 ORDER BY value;",
                 callback, 0, 0);

    printf("\nSELECT COUNT, SUM, AVG:\n");
    sqlite3_exec(db, "SELECT COUNT(*), SUM(value), AVG(value) FROM test;",
                 callback, 0, 0);

    /* Update */
    sqlite3_exec(db, "UPDATE test SET value = value * 2 WHERE id > 3;", 0, 0, 0);
    printf("\nAfter UPDATE (value*2 where id>3):\n");
    sqlite3_exec(db, "SELECT * FROM test;", callback, 0, 0);

    /* Delete */
    sqlite3_exec(db, "DELETE FROM test WHERE id = 2;", 0, 0, 0);
    printf("\nAfter DELETE (id=2):\n");
    sqlite3_exec(db, "SELECT * FROM test;", callback, 0, 0);

    sqlite3_close(db);
    printf("\nDone.\n");
    return 0;
}
