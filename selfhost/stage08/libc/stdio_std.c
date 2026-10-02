/* <stdio.h> over the stream functions: getline, getdelim, tmpfile,
 * perror, fgetpos, fsetpos, setbuf.  Nothing here knows what is inside
 * a FILE (libc/stdio.c does).  Built in phase 2 only: the compiler and
 * the tools use none of it, and stage07 need not compile it. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>

/* A whole line, however long, the newline with it: the buffer is the
 * caller's to free, and is made or grown here as the line needs.  -1 at
 * end of file with nothing read, or when there is no memory. */
ssize_t getdelim(char **lineptr, size_t *n, int delim, FILE *fp) {
    char *buf;
    size_t cap, len;
    int c;

    if (!lineptr || !n || !fp) {
        errno = EINVAL;
        return -1;
    }
    buf = *lineptr;
    cap = buf ? *n : 0;
    len = 0;
    for (;;) {
        c = fgetc(fp);
        if (c == EOF) break;
        if (len + 2 > cap) {
            size_t ncap = cap < 64 ? 128 : cap * 2;
            char *nb = realloc(buf, ncap);
            if (!nb) {
                errno = ENOMEM;
                return -1;
            }
            buf = nb;
            cap = ncap;
            *lineptr = buf;
            *n = cap;
        }
        buf[len++] = (char)c;
        if (c == delim) break;
    }
    if (len == 0) return -1;
    buf[len] = 0;
    return (ssize_t)len;
}

ssize_t getline(char **lineptr, size_t *n, FILE *fp) {
    return getdelim(lineptr, n, '\n', fp);
}

/* A file for reading and writing that has no name: it is made under a
 * name nothing else has, opened, and unlinked at once, so it is gone
 * when it is closed or the run ends.  (The host keeps an unlinked file
 * as long as it is open; one that does not leaves the file behind.) */
FILE *tmpfile(void) {
    static unsigned int serial;
    char name[64];
    struct timespec ts;
    const char *dir;
    FILE *fp;
    int tries;

    for (tries = 0; tries < 16; tries++) {
        ts.tv_sec = 0;
        ts.tv_nsec = 0;
        clock_gettime(CLOCK_REALTIME, &ts);
        serial++;
        dir = tries < 8 ? "/tmp/" : "";
        snprintf(name, sizeof name, "%ss32tmp-%08lx%08lx-%u", dir, (unsigned long)ts.tv_sec,
                 (unsigned long)ts.tv_nsec, serial);
        if (access(name, F_OK) == 0) continue;          /* someone's: another name */
        fp = fopen(name, "w+");
        if (fp) {
            unlink(name);
            return fp;
        }
    }
    return 0;
}

/* "message: what errno means", on stderr */
void perror(const char *s) {
    if (s && *s) {
        fputs(s, stderr);
        fputs(": ", stderr);
    }
    fputs(strerror(errno), stderr);
    fputc('\n', stderr);
}

int fgetpos(FILE *fp, fpos_t *pos) {
    long at = ftell(fp);
    if (at < 0) return -1;
    *pos = at;
    return 0;
}

int fsetpos(FILE *fp, const fpos_t *pos) {
    return fseek(fp, *pos, SEEK_SET);
}

/* setvbuf with the two choices there were before it */
void setbuf(FILE *fp, char *buf) {
    if (buf) setvbuf(fp, buf, _IOFBF, BUFSIZ);
    else setvbuf(fp, 0, _IONBF, 0);
}
