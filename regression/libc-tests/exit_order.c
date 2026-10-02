/* HOSTLEG -- exit runs what atexit registered, the last registered
 * first; one that registers another on the way out has it run next;
 * thirty-two registrations are what the standard promises; and what
 * the streams hold is written after the last of them. */
#include <stdio.h>
#include <stdlib.h>

static int runs;
static void counted(void) { runs++; }
static void last(void) { printf("atexit: registered first, runs last; %d ran between\n", runs); }
static void late(void) { printf("atexit: registered during exit, runs next\n"); }
static void middle(void) { printf("atexit: middle, and registers another\n"); atexit(late); }
static void first(void) { printf("atexit: registered last, runs first\n"); }

int main(void) {
    int i, n;
    n = atexit(last) == 0;
    for (i = 0; i < 29; i++) if (atexit(counted) == 0) n++;
    if (atexit(middle) == 0) n++;
    if (atexit(first) == 0) n++;
    printf("registered %d\n", n);
    printf("main ends with exit(0), this line unflushed and unterminated: ");
    exit(0);
}
