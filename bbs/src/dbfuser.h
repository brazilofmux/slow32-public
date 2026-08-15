#ifndef DBFUSER_H
#define DBFUSER_H

#include <stdio.h>
#include <stdint.h>

typedef struct {
    FILE *fp;
    uint16_t rec_size;
    uint16_t hdr_size;
    uint32_t nrec;
    int name_off;
    int name_len;
    int pass_off;
    int pass_len;
} userdb_t;

int userdb_open(userdb_t *db, const char *path);
void userdb_close(userdb_t *db);
/* 1 = match, 0 = no. Name match is case-insensitive. */
int userdb_check(userdb_t *db, const char *name, const char *pass);

#endif
