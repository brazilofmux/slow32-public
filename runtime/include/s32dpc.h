/* s32dpc.h -- timers, posted reads, and the DPC ring (docs/plans/dpc.md).
 *
 * A DPC is a queue entry, not a function.  Nothing runs inside the instance
 * uninvited.  A timer fires as an entry; a posted read completes as an
 * entry with the bytes already in the caller's own buffer.  The instance
 * is one stack: it posts a flow, keeps going, and looks when it looks.
 * Needs the MMIO libc (libc_mmio.s32a). */
#ifndef S32DPC_H
#define S32DPC_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    unsigned int kind;      /* TIMER_START, POST_READ, or POLL (readiness) */
    unsigned int length;    /* 0 for a timer; bytes transferred for POST_READ */
    unsigned int id;        /* timer id, or guest address of a POST_READ dest */
    unsigned int cookie;    /* what the guest posted with the request */
} s32_dpc_t;

/* Arm a one-shot timer.  Returns its id, or -1 (errno EAGAIN: all
 * S32_MMIO_TIMER_MAX are armed). */
int s32_timer_start(unsigned int seconds, unsigned int nanoseconds, unsigned int cookie);

/* Disarm it.  0, or -1 if the id is not armed (it may already have fired). */
int s32_timer_cancel(int id);

/* Take the next DPC without waiting.  1 with *out filled, 0 if the ring is
 * empty.  Guest-side unread entries are returned first. */
int s32_dpc_poll(s32_dpc_t *out);

/* Put a harvested DPC back so a later poll/wait sees it first.  For an
 * inline waiter that is not the sole consumer (a POST or someone else's
 * READY).  Stale timers are dropped, not unread -- they would loop.
 * 0, or -1 (errno EAGAIN) if the stash is full. */
int s32_dpc_unread(const s32_dpc_t *d);

/* Take the next DPC, sleeping until there is one.  1 with *out filled, or
 * -1 (errno EAGAIN) when nothing is armed (no timer, no pending post) and
 * so nothing could arrive. */
int s32_dpc_wait(s32_dpc_t *out);

/* Post a read as a flow into buf (the caller's own memory, not the MMIO
 * bounce).  Cookie travels in the request scratch.  Bytes are host-owned
 * until the matching DPC is harvested (kind POST_READ, id=(guest)buf,
 * length=nbytes, cookie=cookie).  Returns 0 if the flow was taken, -1 if
 * refused (partition full, bad fd, dest in the code window, DPC ring full
 * on an already-ready fd).  A would-block fd occupies a POST_MAX slot and
 * completes at a later service point -- the instance is not parked. */
int s32_post_read(int fd, void *buf, unsigned n, unsigned cookie);

/* Wait-for-any (readiness, not completion): sleep until a timer fires, a
 * posted read completes, or one of nfds fds (at most S32_DPC_MAX_FDS) is
 * readable.  POLLIN only.  A pending POST_READ owns that fd: the host
 * does not also emit READY, so the bytes are not stolen.  A readable fd
 * arrives as {kind S32_DPC_READY, id=fd, cookie=S32_DPC_IN/HUP/ERR/NVAL},
 * level-triggered.  1 with *out filled, or -1 (errno EAGAIN when nothing
 * is armed and nfds is 0; EINVAL for too many fds). */
int s32_dpc_wait_on(const int *fds, unsigned nfds, s32_dpc_t *out);
#define S32_DPC_TIMER 0x32u     /* a timer fired */
#define S32_DPC_POST  0x0Eu     /* a posted read completed (bytes in the caller's buffer) */
#define S32_DPC_READY 0x34u     /* a named fd is readable (s32_dpc_wait_on) */
#define S32_DPC_IN    1u        /* readable, or at end of file */
#define S32_DPC_HUP   2u        /* the writer went away */
#define S32_DPC_ERR   4u        /* error condition on the fd */
#define S32_DPC_NVAL  8u        /* not an open fd */
#define S32_DPC_MAX_FDS 8       /* fds one wait may name */

#ifdef __cplusplus
}
#endif
#endif
