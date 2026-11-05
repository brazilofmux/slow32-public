/* music.c -- PLAY and SOUND with no speaker: SLOW-32 has no audio, so nothing
 * is heard, but a program sees the rest, as QBasic has it (probed): what a
 * string may say, how long a foreground PLAY keeps it waiting -- until the last
 * note begins, every note before it having been heard out -- what PLAY(n)
 * counts in the background, and when ON PLAY comes. */
#include "music.h"
#include "machine.h"
#include "value.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define QUEUE 64
#define BLOCK_AT 38             /* (a PLAY waits for room with this many) */
#define TICK 0.055              /* (a note has left the count a tick after it began) */

typedef struct { int tempo, length, octave, background; } mstate_t;
static const mstate_t start_state = { 120, 4, 4, 0 };
static mstate_t st = { 120, 4, 4, 0 };
static double q_start[QUEUE], q_end, last_start;
static int q_n;

static double now_s(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + ts.tv_nsec / 1e9;
}

void music_reset(void) {
    st = start_state;
    q_n = 0;
    q_end = last_start = 0;
}

int music_count(void) {
    double now = now_s();
    int k = 0;
    while (k < q_n && q_start[k] + TICK <= now) k++;
    if (k) {
        q_n -= k;
        memmove(q_start, q_start + k, (size_t)q_n * sizeof q_start[0]);
    }
    return q_n;
}

/* the string's notes, parsed before any is queued */
typedef struct { double *dur; int n, cap; mstate_t s; } parse_t;

static error_t note_add(parse_t *p, double seconds) {
    if (p->n == p->cap) {
        int cap = p->cap ? p->cap * 2 : 32;
        double *t = realloc(p->dur, (size_t)cap * sizeof *t);
        if (!t) return ERR_OUT_OF_MEMORY;
        p->dur = t;
        p->cap = cap;
    }
    p->dur[p->n++] = seconds;
    return ERR_NONE;
}

static int upper(int c) { return c >= 'a' && c <= 'z' ? c - 32 : c; }
static void blanks(const char **q, const char *end) {
    while (*q < end && (**q == ' ' || **q == '\t' || **q == ';')) (*q)++;
}

/* a number written, or = and a VARPTR$ of an INTEGER or a SINGLE; 0 none */
static int number(const char **q, const char *end, long *n) {
    const char *r = *q;
    long v = 0;
    int any = 0;
    while (r < end && (*r == ' ' || *r == '\t')) r++;
    if (r < end && *r == '=') {
        if (end - r < 4 || !pc_var_number((unsigned char)r[1], (unsigned char)r[2] | (unsigned char)r[3] << 8, n))
            return -1;
        *q = r + 4;
        return 1;
    }
    while (r < end && *r >= '0' && *r <= '9') {
        if (v < 100000) v = v * 10 + (*r - '0');
        r++;
        any = 1;
    }
    if (!any) return 0;
    *q = r;
    *n = v;
    return 1;
}

/* the note's dots: each half as long again as the last added */
static double dotted(const char **q, const char *end, double d) {
    double add = d / 2;
    while (*q < end && **q == '.') {
        d += add;
        add /= 2;
        (*q)++;
    }
    return d;
}

static error_t parse(parse_t *p, const char *s, int len, int depth) {
    const char *q = s, *end = s + len;
    if (depth > 32) return ERR_ILLEGAL_FUNCTION_CALL;
    for (;;) {
        int c;
        long n;
        blanks(&q, end);
        if (q >= end) return ERR_NONE;
        c = upper((unsigned char)*q++);
        switch (c) {
        case 'T': case 'L': case 'O': case 'P': case 'N': {
            int got = number(&q, end, &n);
            if (got <= 0) return ERR_ILLEGAL_FUNCTION_CALL;
            if (c == 'T') {
                if (n < 32 || n > 255) return ERR_ILLEGAL_FUNCTION_CALL;
                p->s.tempo = (int)n;
            } else if (c == 'L') {
                if (n < 1 || n > 64) return ERR_ILLEGAL_FUNCTION_CALL;
                p->s.length = (int)n;
            } else if (c == 'O') {
                if (n > 6) return ERR_ILLEGAL_FUNCTION_CALL;
                p->s.octave = (int)n;
            } else if (c == 'P') {
                error_t e;
                if (n > 64) return ERR_ILLEGAL_FUNCTION_CALL;
                e = note_add(p, n ? dotted(&q, end, 240.0 / p->s.tempo / (double)n) : 0);
                if (e != ERR_NONE) return e;
            } else {
                error_t e;
                if (n > 84) return ERR_ILLEGAL_FUNCTION_CALL;
                e = note_add(p, dotted(&q, end, 240.0 / p->s.tempo / p->s.length));
                if (e != ERR_NONE) return e;
            }
            break;
        }
        case '<': if (p->s.octave > 0) p->s.octave--; break;
        case '>': if (p->s.octave < 6) p->s.octave++; break;
        case 'M':
            if (q >= end) return ERR_ILLEGAL_FUNCTION_CALL;
            c = upper((unsigned char)*q++);
            if (c == 'B') p->s.background = 1;
            else if (c == 'F') p->s.background = 0;
            else if (c != 'L' && c != 'N' && c != 'S') return ERR_ILLEGAL_FUNCTION_CALL;
            break;
        case 'X': {
            const char *t;
            int tl;
            error_t e;
            if (end - q < 3) return ERR_ILLEGAL_FUNCTION_CALL;
            if ((unsigned char)q[0] != 3) return ERR_TYPE_MISMATCH;
            if (!pc_string_at((unsigned char)q[1] | (unsigned char)q[2] << 8, &t, &tl))
                return ERR_ILLEGAL_FUNCTION_CALL;
            q += 3;
            if ((e = parse(p, t, tl, depth + 1)) != ERR_NONE) return e;
            break;
        }
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': {
            int got;
            double d;
            error_t e;
            n = 0;
            if (q < end && (*q == '+' || *q == '#' || *q == '-')) {
                int flat = *q++ == '-';
                /* (no C flat, B sharp, E sharp or F flat: they have no key of their own) */
                if (flat ? c == 'C' || c == 'F' : c == 'E' || c == 'B') return ERR_ILLEGAL_FUNCTION_CALL;
            }
            got = q < end && *q == '=' ? -1 : number(&q, end, &n);
            if (got < 0 || n > 64) return ERR_ILLEGAL_FUNCTION_CALL;
            d = 240.0 / p->s.tempo / (double)(n ? n : p->s.length);
            e = note_add(p, dotted(&q, end, d));
            if (e != ERR_NONE) return e;
            break;
        }
        default:
            return ERR_ILLEGAL_FUNCTION_CALL;
        }
    }
}

static void wait_until(double t) {
    double now;
    while ((now = now_s()) < t) {
        double left = t - now;
        usleep(left > 0.05 ? 50000 : (unsigned)(left * 1e6));
    }
}

error_t music_play(const char *s, int len) {
    parse_t p;
    error_t e;
    memset(&p, 0, sizeof p);
    p.s = st;
    e = parse(&p, s, len, 0);
    if (e != ERR_NONE) { free(p.dur); return e; }
    st = p.s;
    for (int i = 0; i < p.n; i++) {
        double now;
        while (music_count() >= BLOCK_AT) usleep(5000);
        now = now_s();
        if (q_end < now) q_end = now;
        last_start = q_end;
        if (q_n < QUEUE) q_start[q_n++] = q_end;
        q_end += p.dur[i];
    }
    free(p.dur);
    if (!st.background) wait_until(last_start);
    return ERR_NONE;
}
