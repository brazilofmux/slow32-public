/* sched_mmio.c -- the cooperative scheduler's spawn and run (docs/plans/dpc.md).
 *
 * A single-threaded loop over the DPC ring.  It steps every runnable task
 * once; drains whatever completions are already queued and hands each to the
 * task that awaits it; and, when every live task is blocked, waits on the
 * ring so the host advances the async work and delivers the next one.  The
 * await macros in s32sched.h record what a task waits for (kind + id); this
 * file does the routing. */
#include "s32sched.h"

extern void yield(void);

static s32_task_t g_tasks[S32_SCHED_MAX_TASKS];

int s32_sched_spawn(int (*fn)(s32_task_t *t), void *arg) {
    for (int i = 0; i < S32_SCHED_MAX_TASKS; i++) {
        if (!g_tasks[i].active) {
            g_tasks[i].fn = fn;
            g_tasks[i].arg = arg;
            g_tasks[i].line = 0;
            g_tasks[i].active = 1;
            g_tasks[i].runnable = 1;
            return 0;
        }
    }
    return -1;
}

/* Hand one completion to the task that awaits it; wake that task.  A
 * completion no task awaits is dropped -- it was consumed from the ring, so
 * it cannot spin. */
static void route(const s32_dpc_t *d) {
    for (int i = 0; i < S32_SCHED_MAX_TASKS; i++) {
        if (g_tasks[i].active && !g_tasks[i].runnable &&
            g_tasks[i].wk == d->kind && g_tasks[i].wid == d->id) {
            g_tasks[i].done = *d;
            g_tasks[i].runnable = 1;
            return;
        }
    }
}

/* Every fd a blocked task awaits readable, for the host's wait set. */
static unsigned collect_fds(int *fds) {
    unsigned n = 0;
    for (int i = 0; i < S32_SCHED_MAX_TASKS && n < (unsigned)S32_DPC_MAX_FDS; i++) {
        if (g_tasks[i].active && !g_tasks[i].runnable && g_tasks[i].wk == S32_DPC_READY) {
            fds[n++] = (int)g_tasks[i].wid;
        }
    }
    return n;
}

int s32_sched_run(void) {
    s32_dpc_t d;
    for (;;) {
        /* harvest anything already queued (a task's own yield may have let
         * the host complete an async read) and wake its waiter */
        while (s32_dpc_poll(&d)) route(&d);

        int alive = 0, ran = 0;
        for (int i = 0; i < S32_SCHED_MAX_TASKS; i++) {
            if (!g_tasks[i].active) continue;
            alive++;
            if (!g_tasks[i].runnable) continue;
            g_tasks[i].runnable = 0;
            int r = g_tasks[i].fn(&g_tasks[i]);
            if (r == S32_TASK_DONE) {
                g_tasks[i].active = 0;
            } else if (r == S32_TASK_YIELD) {
                g_tasks[i].runnable = 1;   /* run again next pass */
            }
            /* S32_TASK_WAIT leaves it blocked on wk/wid */
            ran++;
        }

        if (!alive) return 0;
        if (ran) continue;   /* progress made; re-scan (a yielder keeps us busy) */

        /* every live task is blocked: wait on the ring so the host advances
         * the async work and delivers the next completion */
        int fds[S32_DPC_MAX_FDS];
        unsigned nfds = collect_fds(fds);
        int rc = nfds ? s32_dpc_wait_on(fds, nfds, &d) : s32_dpc_wait(&d);
        if (rc < 0) return -1;   /* nothing can arrive: a task is stuck for good */
        if (rc == 1) route(&d);
    }
}
