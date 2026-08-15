#ifndef DBFMSG_H
#define DBFMSG_H

#include <stdio.h>
#include <stdint.h>

#define MSG_FROM_MAX 16
#define MSG_TO_MAX   16
#define MSG_SUBJ_MAX 40
#define MSG_TEXT_MAX 254

typedef struct {
    char from[MSG_FROM_MAX + 1];
    char to[MSG_TO_MAX + 1];
    char subj[MSG_SUBJ_MAX + 1];
    char text[MSG_TEXT_MAX + 1];
} msg_t;

typedef struct {
    FILE *fp;
    uint16_t rec_size;
    uint16_t hdr_size;
    uint32_t nrec;
    int from_off, from_len;
    int to_off, to_len;
    int subj_off, subj_len;
    int text_off, text_len;
} msgdb_t;

int msgdb_open(msgdb_t *db, const char *path);
void msgdb_close(msgdb_t *db);
uint32_t msgdb_count(const msgdb_t *db);
/* recno is 1-based. Returns 0 on success. */
int msgdb_get(msgdb_t *db, uint32_t recno, msg_t *out);
int msgdb_post(msgdb_t *db, const char *from, const char *to,
               const char *subj, const char *text);

#endif
