/* A cooperative scheduler over the DPC ring (docs/plans/dpc.md): three tasks
 * in one instance, run together, a task's work overlapping another's I/O.
 *
 *  - reader: submits one async read of stdin and awaits it.  stdin is the
 *    pipe in stdin.sh: nothing for half a second, then a line.
 *  - ticker: awaits three short (5 ms) timers in turn.  They all fire long
 *    before the read completes, so each tick sees the read still pending --
 *    which is the proof that the scheduler kept the read in flight while it
 *    ran the ticker, rather than serializing the two.
 *  - worker: a compute-bound task that yields between chunks, so it does not
 *    starve the instance's I/O.  It prints nothing; its sum is checked after.
 *
 * The output is deterministic: the three ticks (<=15 ms) always precede the
 * reader's completion (500 ms), and the worker's sum is a fixed number. */
#include <stdio.h>
#include "s32sched.h"

#define RBUF_MAX 64u

typedef struct { int ticks; int reader_done; } shared_t;
typedef struct { shared_t *sh; int i; } ticker_state;
typedef struct { shared_t *sh; int nbytes; char buf[RBUF_MAX]; } reader_state;
typedef struct { shared_t *sh; unsigned sum; int chunk; } worker_state;

static int reader_task(s32_task_t *t) {
    reader_state *s = t->arg;
    S32_ASYNC_BEGIN(t);
    S32_AWAIT_READ(t, 0, s->buf, RBUF_MAX, &s->nbytes);
    s->sh->reader_done = 1;
    printf("reader done: %d bytes: %.*s", s->nbytes, s->nbytes, s->buf);
    S32_ASYNC_END(t);
}

static int ticker_task(s32_task_t *t) {
    ticker_state *s = t->arg;
    S32_ASYNC_BEGIN(t);
    for (s->i = 0; s->i < 3; s->i++) {
        S32_AWAIT_TIMER(t, 0, 5000000u);          /* 5 ms */
        s->sh->ticks++;
        printf("tick %d (reader %s)\n", s->i + 1,
               s->sh->reader_done ? "done" : "pending");
    }
    S32_ASYNC_END(t);
}

static int worker_task(s32_task_t *t) {
    worker_state *s = t->arg;
    S32_ASYNC_BEGIN(t);
    s->sum = 0;
    for (s->chunk = 0; s->chunk < 10; s->chunk++) {
        for (int j = 0; j < 100000; j++) {
            s->sum += (unsigned)(s->chunk * 100000 + j);
        }
        S32_YIELD(t);                              /* let the host run I/O */
    }
    S32_ASYNC_END(t);
}

int main(void) {
    shared_t sh = { 0, 0 };
    reader_state rs = { &sh, 0 };
    ticker_state ts = { &sh, 0 };
    worker_state ws = { &sh, 0, 0 };

    s32_sched_spawn(reader_task, &rs);
    s32_sched_spawn(ticker_task, &ts);
    s32_sched_spawn(worker_task, &ws);
    int rc = s32_sched_run();

    printf("worker sum=%u\n", ws.sum);
    printf("scheduler: %d ticks, reader %s, rc=%d\n",
           sh.ticks, sh.reader_done ? "done" : "pending", rc);
    return 0;
}
