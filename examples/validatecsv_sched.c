/* validatecsv_sched.c -- the CSV validator on the cooperative scheduler.
 *
 * Same validation as validatecsv.c (the serial baseline), same output byte
 * for byte, but structured as the SLOW-32 cluster's level 1: one task per
 * file, run cooperatively, each task streaming its file through the FSM a
 * chunk at a time.  A task's next chunk-read (S32_AWAIT_READ / POST_READ) is
 * a scheduler yield point, so one file's read overlaps another's parse -- and
 * streaming keeps each task's memory bounded, the instance's heap unshared
 * and small.  The FSM's whole state is a handful of ints, so it streams for
 * free.
 *
 * This is the single-instance slice: it proves the parser ports onto the
 * scheduler and that the shape is clean.  It does NOT beat the serial version
 * on warm files -- one instance is one thread of control, so tasks interleave
 * rather than run in parallel; the speedup is the multi-instance step.  See
 * docs/plans/hosting.md.
 *
 * Build:  ../slow32cc --libc=mmio -O2 validatecsv_sched.c -o validatecsv_sched.s32x
 */
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "s32sched.h"

/* byte -> column in the transition tables: Any, LF, CR, ',', '"' */
static inline uint8_t classify_byte(uint8_t b) {
    switch (b) {
    case '\n': return 1;
    case '\r': return 2;
    case ',':  return 3;
    case '"':  return 4;
    default:   return 0;
    }
}

static const uint8_t s32_stt_loose[13][5] = {
    {  6, 10,  3,  8,  1 }, {  7,  7,  7,  7,  2 }, {  6, 11,  4,  8,  5 },
    { 12, 10, 12, 12, 12 }, { 12, 11, 12, 12, 12 }, {  7,  7,  7,  7,  2 },
    {  6, 11,  4,  8,  6 }, {  7,  7,  7,  7,  2 }, {  6, 11,  9,  8,  1 },
    { 12, 10, 12, 12, 12 }, {  6, 10,  3,  8,  1 }, {  6, 10,  3,  8,  1 },
    { 12, 12, 12, 12, 12 },
};
static const uint8_t s32_stt_strict[13][5] = {
    {  6, 10,  3,  8,  1 }, {  7,  7,  7,  7,  2 }, { 12, 11,  4,  8,  5 },
    { 12, 10, 12, 12, 12 }, { 12, 11, 12, 12, 12 }, {  7,  7,  7,  7,  2 },
    {  6, 11,  4,  8, 12 }, {  7,  7,  7,  7,  2 }, {  6, 11,  9,  8,  1 },
    { 12, 10, 12, 12, 12 }, {  6, 10,  3,  8,  1 }, {  6, 10,  3,  8,  1 },
    { 12, 12, 12, 12, 12 },
};

#define CHUNK      512u
#define ERRBUF_CAP 4096u

/* One file's validation, as a task's own state.  Locals do not survive an
 * await, so everything the task carries across S32_AWAIT_READ lives here. */
typedef struct {
    const char *filename;
    bool strict;

    int fd;
    unsigned char chunk[CHUNK];
    int nbytes;
    bool eof;
    bool abort;

    /* FSM */
    uint8_t state;
    bool have_expected;
    int expected_fields;
    int fields;
    int line_number;
    bool valid;
    bool any_bytes;

    /* buffered output, so the driver can emit in file order */
    char err[ERRBUF_CAP];
    unsigned err_len;
    bool result_ready;
} vstate;

static void verr(vstate *s, const char *fmt, ...) {
    if (s->err_len >= ERRBUF_CAP - 1u) return;
    va_list ap;
    va_start(ap, fmt);
    int n = vsnprintf(s->err + s->err_len, ERRBUF_CAP - s->err_len, fmt, ap);
    va_end(ap);
    if (n > 0) {
        s->err_len += (unsigned)n;
        if (s->err_len > ERRBUF_CAP - 1u) s->err_len = ERRBUF_CAP - 1u;
    }
}

/* Feed one chunk through the FSM -- plain code, no awaits.  Mirrors the byte
 * loop in validatecsv.c exactly, but buffers errors instead of printing. */
static void feed_chunk(vstate *s, int n) {
    for (int i = 0; i < n && !s->abort; ++i) {
        s->any_bytes = true;
        uint8_t column = classify_byte(s->chunk[i]);
        s->state = s->strict ? s32_stt_strict[s->state][column]
                             : s32_stt_loose[s->state][column];
        if (s->state <= 7) continue;
        switch (s->state) {
        case 8:
        case 9:
            ++s->fields;
            break;
        case 10:
        case 11:
            if (s->state == 11) ++s->fields;
            if (s->have_expected) {
                if (s->fields != s->expected_fields) {
                    s->valid = false;
                    verr(s, "%s: Line %d contains %d fields instead of the expected %d fields.\n",
                         s->filename, s->line_number, s->fields, s->expected_fields);
                }
            } else {
                s->expected_fields = s->fields;
                s->have_expected = true;
            }
            s->fields = 0;
            ++s->line_number;
            break;
        case 12:
            s->valid = false;
            verr(s, "%s: Unexpected character '%c' (decimal %d) on line %d.\n",
                 s->filename, (char)s->chunk[i], (int)s->chunk[i], s->line_number);
            s->abort = true;
            break;
        default:
            break;
        }
    }
}

/* End-of-file finish -- mirrors the tail of validatecsv.c. */
static void finish_fsm(vstate *s) {
    if (!s->any_bytes) { s->valid = true; return; }   /* empty file */
    if (!s->valid) return;
    switch (s->state) {
    case 6:
    case 7:
    case 8:
    case 9:
        ++s->fields;
        if (s->have_expected) {
            if (s->fields != s->expected_fields) {
                s->valid = false;
                verr(s, "%s: Line %d contains %d fields instead of the expected %d fields.\n",
                     s->filename, s->line_number, s->fields, s->expected_fields);
            }
        } else {
            s->expected_fields = s->fields;
            s->have_expected = true;
        }
        s->fields = 0;
        ++s->line_number;
        break;
    case 0:
    case 10:
    case 11:
        break;
    default:
        s->valid = false;
        verr(s, "%s: Unexpected end of file while parsing line %d.\n",
             s->filename, s->line_number);
        break;
    }
}

static int validate_task(s32_task_t *t) {
    vstate *s = t->arg;
    S32_ASYNC_BEGIN(t);

    /* one exit only: a protothread cannot take an early S32_ASYNC_END (it
     * closes the switch).  A failed open just skips the read loop. */
    s->fd = open(s->filename, O_RDONLY);
    if (s->fd < 0) {
        verr(s, "Unable to open file: %s\n", s->filename);
        s->valid = false;
    }

    while (s->fd >= 0 && !s->eof && !s->abort) {
        S32_AWAIT_READ(t, s->fd, s->chunk, CHUNK, &s->nbytes);
        if (s->nbytes <= 0) {
            s->eof = true;
        } else {
            feed_chunk(s, s->nbytes);
        }
    }

    if (s->fd >= 0) {
        finish_fsm(s);
        close(s->fd);
    }
    s->result_ready = true;
    S32_ASYNC_END(t);
}

static void init_vstate(vstate *s, const char *filename, bool strict) {
    memset(s, 0, sizeof(*s));
    s->filename = filename;
    s->strict = strict;
    s->fd = -1;
    s->state = 0;
    s->line_number = 1;
    s->valid = true;
}

int main(int argc, char **argv) {
    bool strict = false;
    const char *files[256];
    int nfiles = 0;

    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-s") == 0) { strict = true; continue; }
        if (nfiles < 256) files[nfiles++] = argv[i];
    }
    if (nfiles == 0) {
        printf("Usage: validatecsv [-s] <filename1> [filename2] ...\n");
        return 1;
    }

    static vstate states[S32_SCHED_MAX_TASKS];
    bool overall_valid = true;

    /* Batches of at most S32_SCHED_MAX_TASKS files run together; results are
     * emitted in file order after each batch, so the output matches the
     * serial validator byte for byte regardless of how the tasks interleave. */
    for (int base = 0; base < nfiles; base += S32_SCHED_MAX_TASKS) {
        int n = nfiles - base;
        if (n > S32_SCHED_MAX_TASKS) n = S32_SCHED_MAX_TASKS;

        for (int k = 0; k < n; ++k) {
            init_vstate(&states[k], files[base + k], strict);
            s32_sched_spawn(validate_task, &states[k]);
        }
        s32_sched_run();

        for (int k = 0; k < n; ++k) {
            vstate *s = &states[k];
            if (s->err_len) fputs(s->err, stdout);
            if (!s->valid) {
                printf("%s: File is %s invalid with %d columns and %d rows.\n",
                       s->filename, strict ? "strictly" : "loosely",
                       s->expected_fields, s->line_number);
                overall_valid = false;
            }
        }
    }

    return overall_valid ? 0 : 1;
}
