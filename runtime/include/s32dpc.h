/* s32dpc.h -- timers and the DPC ring (docs/plans/dpc.md).
 *
 * A DPC is a queue entry, not a function.  A timer armed here fires as an
 * entry in the instance's DPC ring; the instance reads it when it looks
 * (s32_dpc_poll) or sleeps until one is there (s32_dpc_wait).  Nothing runs
 * inside the instance uninvited.  Needs the MMIO libc (libc_mmio.s32a). */
#ifndef S32DPC_H
#define S32DPC_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    unsigned int kind;      /* the request that produced it: S32_MMIO_OP_TIMER_START */
    unsigned int length;    /* 0 for a timer */
    unsigned int id;        /* the timer id */
    unsigned int cookie;    /* what the guest armed it with */
} s32_dpc_t;

/* Arm a one-shot timer.  Returns its id, or -1 (errno EAGAIN: all
 * S32_MMIO_TIMER_MAX are armed). */
int s32_timer_start(unsigned int seconds, unsigned int nanoseconds, unsigned int cookie);

/* Disarm it.  0, or -1 if the id is not armed (it may already have fired). */
int s32_timer_cancel(int id);

/* Take the next DPC without waiting.  1 with *out filled, 0 if the ring is
 * empty.  No host involvement: a load of the ring's head word. */
int s32_dpc_poll(s32_dpc_t *out);

/* Take the next DPC, sleeping until there is one.  1 with *out filled, or
 * -1 (errno EAGAIN) when nothing is armed and so nothing could arrive. */
int s32_dpc_wait(s32_dpc_t *out);

#ifdef __cplusplus
}
#endif
#endif
