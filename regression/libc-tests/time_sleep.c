/* HOSTLEG -- the clocks and the sleeps: time passes while a program
 * sleeps, by about as much as it asked for. */
#include <stdio.h>
#include <time.h>
#include <unistd.h>

static long long ms_between(const struct timespec *a, const struct timespec *b) {
    return ((long long)b->tv_sec - (long long)a->tv_sec) * 1000 + ((long long)b->tv_nsec - (long long)a->tv_nsec) / 1000000;
}

int main(void) {
    struct timespec t0, t1, req, rem;
    long long ms;
    time_t now;
    int r;

    r = clock_gettime(CLOCK_REALTIME, &t0);
    now = time(0);
    printf("clock_gettime %d, agrees with time(): %d, nanoseconds in range: %d\n", r,
           (long long)t0.tv_sec - (long long)now >= -1 && (long long)t0.tv_sec - (long long)now <= 1,
           t0.tv_nsec >= 0 && t0.tv_nsec < 1000000000);

    req.tv_sec = 0;
    req.tv_nsec = 30000000;
    rem.tv_sec = 99;
    rem.tv_nsec = 99;
    r = nanosleep(&req, &rem);
    clock_gettime(CLOCK_REALTIME, &t1);
    ms = ms_between(&t0, &t1);
    printf("nanosleep(30ms) %d, slept at least 30ms: %d, and not a second: %d\n", r, ms >= 30, ms < 1000);
    r = nanosleep(&req, 0);
    printf("nanosleep with no remainder asked for: %d\n", r);

    req.tv_nsec = 1000000000;
    printf("nanosleep with a billion nanoseconds: %s\n", nanosleep(&req, 0) != 0 ? "refused" : "ACCEPTED");

    clock_gettime(CLOCK_REALTIME, &t0);
    r = usleep(20000);
    clock_gettime(CLOCK_REALTIME, &t1);
    ms = ms_between(&t0, &t1);
    printf("usleep(20000) %d, slept at least 20ms: %d\n", r, ms >= 20);
    printf("sleep(0) %u\n", sleep(0));
    clock_gettime(CLOCK_REALTIME, &t0);
    printf("sleep(1) %u", sleep(1));
    clock_gettime(CLOCK_REALTIME, &t1);
    ms = ms_between(&t0, &t1);
    printf(", slept at least a second: %d, and not three: %d\n", ms >= 1000, ms < 3000);
    return 0;
}
