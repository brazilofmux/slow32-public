/* CLOCK_MONOTONIC (GETTIME's request status 1, 2026-10-20): a clock that
 * never goes back, which C++'s steady_clock and every timeout are built on.
 * Its readings must not decrease; it must not be the wall clock under
 * another name only in the sense that both answer -- the values are not
 * compared, a host may start them anywhere -- but an unknown clock id must
 * be refused, by clock_gettime and by the request itself.  Status 2 is the
 * processor time (CLOCK_PROCESS_CPUTIME_ID, clock()), which work advances. */
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include "../../../runtime/mmio_ring.h"

static int fails;
static void ok(int cond, const char *what)
{
    printf("%s %s\n", cond ? "ok" : "FAIL", what);
    if (!cond) fails++;
}

static int before(const struct timespec *a, const struct timespec *b)
{
    return a->tv_sec < b->tv_sec || (a->tv_sec == b->tv_sec && a->tv_nsec < b->tv_nsec);
}

int main(void)
{
    struct timespec prev, now, wall;
    ok(clock_gettime(CLOCK_MONOTONIC, &prev) == 0, "monotonic clock answers");
    ok(prev.tv_nsec < 1000000000L, "monotonic nanoseconds in range");
    int backwards = 0;
    for (int i = 0; i < 2000; i++) {
        if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) { backwards = -1; break; }
        if (before(&now, &prev)) backwards++;
        prev = now;
    }
    ok(backwards == 0, "2000 readings never go back");
    ok(clock_gettime(CLOCK_REALTIME, &wall) == 0 && wall.tv_sec > 1700000000ull,
       "wall clock still the wall clock");

    errno = 0;
    ok(clock_gettime(7, &now) == -1 && errno == EINVAL, "unknown clock id is EINVAL");
    errno = 0;
    unsigned r = (unsigned)s32_mmio_request(S32_MMIO_OP_GETTIME, 16u, 0u, 3u);
    ok(r == 0xFFFFFFFFu && errno == EINVAL, "GETTIME status 3 is EINVAL");

    struct timespec c0, c1;
    ok(clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &c0) == 0, "processor-time clock answers");
    clock_t k0 = clock();
    volatile unsigned spin = 0;
    for (unsigned i = 0; i < 20000000u; i++) spin += i;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &c1);
    clock_t k1 = clock();
    ok(before(&c0, &c1), "work advances it");
    ok(k0 != (clock_t)-1 && k1 > k0, "and clock() with it");
    printf("%d failed\n", fails);
    return fails;
}
