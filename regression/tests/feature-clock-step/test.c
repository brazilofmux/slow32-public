/* The step clock (S32_CLOCK=step, docs/plans/input-queue.md, the first
 * edge): time a test owns.  env.txt starts it at 1987-01-01T00:00:00Z.
 * It moves one quantum (1 ms by default) at every service point, by a
 * SLEEP's interval at once, and to the deadline when a wait would block --
 * so every engine prints these lines, and none of them waits: the test's
 * four seconds of sleeping pass in no time.
 *
 * The intervals are checked to within a few quanta: each call that asks
 * the host is a service point, and so a millisecond. */
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <poll.h>
#include "s32dpc.h"

static int fails;
static void ok(int cond, const char *what)
{
    printf("%s %s\n", cond ? "ok" : "FAIL", what);
    if (!cond) fails++;
}

static long long mono_ns(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

/* d is ms, give or take the service points around it */
static int about(long long d_ns, long long ms)
{
    return d_ns >= ms * 1000000LL && d_ns < (ms + 10) * 1000000LL;
}

int main(void)
{
    time_t t = time(NULL);
    ok(t == 536457600, "the wall clock starts at S32_CLOCK_EPOCH");
    char day[32];
    strftime(day, sizeof day, "%Y-%m-%d %H:%M", gmtime(&t));
    printf("today is %s\n", day);

    long long a = mono_ns(), b = mono_ns();
    ok(b - a == 1000000LL, "one service point is one 1 ms quantum");

    a = mono_ns();
    sleep(2);
    ok(about(mono_ns() - a, 2000), "sleep(2) moves the clock 2 s");

    a = mono_ns();
    usleep(250000);
    ok(about(mono_ns() - a, 250), "usleep(250000) moves it 250 ms");

    a = mono_ns();
    int id = s32_timer_start(1u, 0u, 7u);
    s32_dpc_t d;
    int got = s32_dpc_wait(&d);
    ok(id >= 0 && got == 1 && d.kind == S32_DPC_TIMER && d.cookie == 7u,
       "a 1 s timer fires");
    ok(about(mono_ns() - a, 1000), "the wait for it jumps the clock 1 s");

    a = mono_ns();
    ok(poll(NULL, 0, 300) == 0, "poll with nothing to watch times out");
    ok(about(mono_ns() - a, 300), "and its 300 ms pass at once");

    ok(s32_dpc_wait(&d) == -1, "with nothing armed a wait still ends (EAGAIN)");

    t = time(NULL);
    ok(t == 536457600 + 3 || t == 536457600 + 4, "the wall clock moved with it");

    printf("%s\n", fails ? "FAILED" : "all ok");
    return fails != 0;
}
