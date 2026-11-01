/* 7.22.4.4 exit: a function registered while the handlers are running still gets called */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void late(void) { printf("late\n"); }
static void early(void) { printf("early\n"); atexit(late); }
static void first(void) { printf("first\n"); }
int main(void) {
    atexit(first);
    atexit(early);
    return 0;
}
