/* 7.2.1.1p2: a failing assert writes to stderr, not stdout, naming the expression text, the file (its last path component is enough), the line and the enclosing function */
#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int fail_line;
static char text[1024];

static int has(const char *s) { return strstr(text, s) != 0; }

static void on_abrt(int sig) {
    char line[16];
    FILE *f;
    size_t n;
    const char *base;
    (void)sig;
    fflush(stderr);
    f = fopen("err.txt", "r");
    n = f ? fread(text, 1, sizeof text - 1, f) : 0;
    text[n] = 0;
    sprintf(line, "%d", fail_line);
    printf("stderr written: %d\n", n > 0);
    printf("expression: %d\n", has("probe_flag == 7"));
    base = strrchr(__FILE__, '/');
    printf("file: %d\n", has(base ? base + 1 : __FILE__));
    printf("line: %d\n", has(line));
    printf("function: %d\n", has("check_it"));
    fflush(stdout);
    _Exit(0);
}

static int probe_flag;

static void check_it(void) {
    fail_line = __LINE__ + 1;
    assert(probe_flag == 7);
}

int main(void) {
    if (!freopen("err.txt", "w", stderr))
        return 1;
    signal(SIGABRT, on_abrt);
    check_it();
    return 2;
}
