/* timespec_get (C11 7.27.2.5), for both C libraries: the calendar time
 * in seconds and nanoseconds; TIME_UTC is the one base. */
#include <time.h>

int timespec_get(struct timespec *ts, int base) {
    if (base != TIME_UTC || !ts) return 0;
    if (clock_gettime(CLOCK_REALTIME, ts) != 0) return 0;
    return base;
}
