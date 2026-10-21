#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int main() {
    int errors = 0;

    // Test abs/labs
    if (abs(-123) != 123) { printf("abs(-123) failed\n"); errors++; }
    if (abs(123) != 123) { printf("abs(123) failed\n"); errors++; }
    if (labs(-12345L) != 12345L) { printf("labs failed\n"); errors++; }

    // Test atoi/atol
    if (atoi("12345") != 12345) { printf("atoi failed\n"); errors++; }
    if (atoi("-123") != -123) { printf("atoi negative failed\n"); errors++; }
    if (atol("12345678") != 12345678L) { printf("atol failed\n"); errors++; }

    // Test rand
    srand(123);
    int r1 = rand();
    int r2 = rand();
    if (r1 == r2) { printf("rand() returned same value twice (unlikely)\n"); errors++; }
    srand(123);
    if (rand() != r1) { printf("srand() didn't reset sequence\n"); errors++; }

    // Test array of pointers
    const char *test_arr[] = {"One", "Two"};
    printf("Test array: %s\n", test_arr[1]);
    if (strcmp(test_arr[1], "Two") != 0) {
        printf("String array access failed\n");
        errors++;
    }

    // Test time functions
    // 2026-01-29 12:00:00 UTC
    // Epoch: 1769688000 (approx)
    // Let's use a known fixed timestamp
    // 2000-01-01 00:00:00 UTC = 946684800
    time_t t = 946684800;
    
    struct tm *tm = gmtime(&t);
    if (!tm) { printf("gmtime failed\n"); errors++; }
    else {
        if (tm->tm_year != 100) { printf("gmtime year wrong: %d\n", tm->tm_year); errors++; }
        if (tm->tm_mon != 0) { printf("gmtime mon wrong: %d\n", tm->tm_mon); errors++; }
        if (tm->tm_mday != 1) { printf("gmtime mday wrong: %d\n", tm->tm_mday); errors++; }
        if (tm->tm_hour != 0) { printf("gmtime hour wrong: %d\n", tm->tm_hour); errors++; }
    }

    // Round trip mktime
    if (tm) {
        time_t t2 = mktime(tm);
        if (t2 != t) { printf("mktime roundtrip failed: %%lld != %%lld\n", (long long)t2, (long long)t); errors++; }
    }

    // Test asctime
    if (tm) {
        char *s = asctime(tm);
        // "Sat Jan  1 00:00:00 2000\n"
        if (strstr(s, "Sat Jan  1 00:00:00 2000") == NULL) {
            printf("asctime format wrong: '%s'\n", s);
            errors++;
        }
    }

    /* libc_debug: the clock and sleep exist as symbols; without MMIO they
     * refuse.  A bad clock id or a null timespec is EINVAL either way. */
    {
        struct timespec ts, req;
        errno = 0;
        if (!(clock_gettime(CLOCK_REALTIME, &ts) == -1 && errno == ENOSYS)) {
            printf("clock_gettime REALTIME should be ENOSYS\n"); errors++;
        }
        errno = 0;
        if (!(clock_gettime(CLOCK_REALTIME, 0) == -1 && errno == EINVAL)) {
            printf("clock_gettime NULL ts should be EINVAL\n"); errors++;
        }
        errno = 0;
        if (!(clock_gettime(7, &ts) == -1 && errno == EINVAL)) {
            printf("clock_gettime bad clock should be EINVAL\n"); errors++;
        }
        req.tv_sec = 0;
        req.tv_nsec = 1000000000;
        errno = 0;
        if (!(nanosleep(&req, 0) == -1 && errno == EINVAL)) {
            printf("nanosleep 1e9 nsec should be EINVAL\n"); errors++;
        }
        errno = 0;
        if (!(nanosleep(0, 0) == -1 && errno == EINVAL)) {
            printf("nanosleep NULL req should be EINVAL\n"); errors++;
        }
    }

    if (errors == 0) {
        printf("All stdlib/time tests passed\n");
    } else {
        printf("Failed %d tests\n", errors);
    }

    return errors != 0;
}
