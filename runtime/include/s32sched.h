/* s32sched.h -- a cooperative scheduler over the DPC ring (docs/plans/dpc.md).
 *
 * The customer the DPC work was building toward: many tasks in one instance,
 * run cooperatively, so a task's computation overlaps another's I/O.  It is
 * ordinary guest code -- no preemption, no threads, no extra stacks.  Each
 * task is a protothread: a function that runs to an await point and returns
 * to the scheduler, its resume point remembered.  The scheduler dispatches
 * DPC ring entries to the task that awaits them, and yields to the host so
 * the async work advances.  One instance is still a single-threaded consumer
 * of its queue; the scheduler IS that consumer.
 *
 * A task keeps its own state across awaits in the struct its arg points at,
 * NOT in locals: an await returns from the function, so a local does not
 * survive it.  This is the protothread discipline.  One task waits on a given
 * (kind, id) at a time.  Needs the MMIO libc (libc_mmio.s32a).
 */
#ifndef S32SCHED_H
#define S32SCHED_H

#include "s32dpc.h"

#ifdef __cplusplus
extern "C" {
#endif

#define S32_SCHED_MAX_TASKS 8

/* What a task function returns to the scheduler. */
#define S32_TASK_DONE  0   /* finished; the slot frees */
#define S32_TASK_WAIT  1   /* blocked on t->wk/t->wid until its DPC arrives */
#define S32_TASK_YIELD 2   /* gave up the CPU; run me again, the host was serviced */

typedef struct s32_task {
    int (*fn)(struct s32_task *t);
    void *arg;              /* the task's own state; survives awaits */
    int line;               /* protothread resume point (0 = start) */
    unsigned wk;            /* what it awaits: S32_DPC_TIMER / _AIO / _READY */
    unsigned wid;           /* the id it awaits: timer id, aio id, or fd */
    s32_dpc_t done;         /* the completion, filled before the task resumes */
    unsigned char active;
    unsigned char runnable;
} s32_task_t;

/* Register a task.  fn is a protothread (S32_ASYNC_BEGIN..END); arg is its
 * state.  Returns 0, or -1 if all S32_SCHED_MAX_TASKS slots are taken. */
int s32_sched_spawn(int (*fn)(s32_task_t *t), void *arg);

/* Run until every task is done.  Returns 0, or -1 on deadlock (a task is
 * blocked on something that can never arrive). */
int s32_sched_run(void);

/* ---- the protothread body, for a task function ---------------------------
 * int my_task(s32_task_t *t) {
 *     my_state *s = t->arg;
 *     S32_ASYNC_BEGIN(t);
 *     ... S32_AWAIT_TIMER(t, 0, 5000000u); ...
 *     S32_ASYNC_END(t);
 * }
 */
#define S32_ASYNC_BEGIN(t)  switch ((t)->line) { case 0:
#define S32_ASYNC_END(t)    } (t)->line = -1; return S32_TASK_DONE;

/* Arm a one-shot timer and suspend until it fires. */
#define S32_AWAIT_TIMER(t, sec, nsec)                                     \
    do {                                                                  \
        (t)->wk = S32_DPC_TIMER;                                          \
        (t)->wid = (unsigned)s32_timer_start((sec), (nsec), 0u);          \
        (t)->line = __LINE__; return S32_TASK_WAIT; case __LINE__: ;      \
    } while (0)

/* Post a read into buf (the caller's own memory) and suspend until it
 * completes; *pbytes gets the byte count (0 = end of file).  The bytes are
 * in buf when the task resumes -- POST_READ writes it directly, so no copy
 * out of a bounce buffer.  The completion is routed by the dest address. */
#define S32_AWAIT_READ(t, fd, buf, n, pbytes)                             \
    do {                                                                  \
        (t)->wk = S32_DPC_POST;                                           \
        (t)->wid = (unsigned)(unsigned long)(buf);                        \
        s32_post_read((fd), (buf), (n), 0u);                              \
        (t)->line = __LINE__; return S32_TASK_WAIT; case __LINE__: ;      \
        *(pbytes) = (int)(t)->done.length;                                \
    } while (0)

/* Give up the CPU: the host is serviced (async reads advance, timers are
 * checked) and the task is run again.  For a compute-bound task that must not
 * starve the instance's I/O. */
#define S32_YIELD(t)                                                      \
    do {                                                                  \
        extern void yield(void);                                          \
        yield();                                                          \
        (t)->line = __LINE__; return S32_TASK_YIELD; case __LINE__: ;     \
    } while (0)

#ifdef __cplusplus
}
#endif
#endif
