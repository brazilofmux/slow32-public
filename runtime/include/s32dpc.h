/* s32dpc.h -- timers, posted reads, and the DPC ring (docs/plans/dpc.md).
 *
 * A DPC is a queue entry, not a function.  Nothing runs inside the instance
 * uninvited.  A timer fires as an entry; a posted read completes as an
 * entry with the bytes already in the MMIO data buffer.  The instance is
 * one stack: it posts a flow, keeps going, and looks when it looks.  Needs
 * the MMIO libc (libc_mmio.s32a). */
#ifndef S32DPC_H
#define S32DPC_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    unsigned int kind;      /* TIMER_START or POST_READ */
    unsigned int length;    /* 0 for a timer; bytes transferred for POST_READ */
    unsigned int id;        /* timer id, or DATA_BUFFER dest of a POST_READ */
    unsigned int cookie;    /* what the guest posted with the request */
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

/* Post a read as a flow.  dest is an offset in the MMIO data buffer; the
 * first four bytes there are overwritten with cookie for the host to save,
 * then with the file bytes.  Those bytes are host-owned until the matching
 * DPC is harvested (kind POST_READ, id=dest, length=nbytes, cookie=cookie).
 * Returns 0 if the flow was taken, -1 if it was refused (would block,
 * bad fd, DPC ring full).  The instance is not parked on the fd. */
int s32_post_read(int fd, unsigned dest, unsigned n, unsigned cookie);

/* Copy n bytes from a POST_READ dest in the MMIO data buffer.  Call after
 * harvesting the DPC for that dest. */
void s32_post_copy(unsigned dest, void *buf, unsigned n);

#ifdef __cplusplus
}
#endif
#endif
