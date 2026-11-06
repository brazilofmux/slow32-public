/* HOSTLEG -- setenv, unsetenv, putenv over a host environment the guest cannot
 * change, mkstemp and the rest of what flang-rt asked for.  Only what POSIX
 * fixes is printed. */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/times.h>
#include <unistd.h>

static void show(const char *name) {
    const char *v = getenv(name);
    printf("%s=%s\n", name, v ? v : "(unset)");
}

int main(void) {
    show("S32_NOT_SET_ANYWHERE");
    printf("set %d\n", setenv("S32_NOT_SET_ANYWHERE", "first", 1));
    show("S32_NOT_SET_ANYWHERE");
    const char *kept = getenv("S32_NOT_SET_ANYWHERE");
    printf("keep %d\n", setenv("S32_NOT_SET_ANYWHERE", "ignored", 0));
    show("S32_NOT_SET_ANYWHERE");
    printf("replace %d\n", setenv("S32_NOT_SET_ANYWHERE", "second", 1));
    show("S32_NOT_SET_ANYWHERE");
    printf("empty %d\n", setenv("S32_EMPTY", "", 1));
    show("S32_EMPTY");
    printf("unset %d\n", unsetenv("S32_NOT_SET_ANYWHERE"));
    show("S32_NOT_SET_ANYWHERE");
    printf("unset-missing %d\n", unsetenv("S32_NEVER_THERE"));
    printf("set-again %d\n", setenv("S32_NOT_SET_ANYWHERE", "third", 0));
    show("S32_NOT_SET_ANYWHERE");
    (void)kept;

    /* a variable the host has, which the guest then changes and removes */
    printf("home-there %d\n", getenv("HOME") != NULL);
    printf("home-keep %d\n", setenv("HOME", "/nowhere", 0));
    printf("home-same %d\n", strcmp(getenv("HOME"), "/nowhere") != 0);
    printf("home-replace %d\n", setenv("HOME", "/nowhere", 1));
    show("HOME");
    printf("home-unset %d\n", unsetenv("HOME"));
    show("HOME");

    /* the string itself is the entry */
    static char mine[] = "S32_PUT=one";
    printf("put %d\n", putenv(mine));
    show("S32_PUT");
    mine[8] = 't'; mine[9] = 'w'; mine[10] = 'o';
    show("S32_PUT");

    errno = 0;
    printf("bad-empty %d %d\n", setenv("", "x", 1), errno == EINVAL);
    errno = 0;
    printf("bad-eq %d %d\n", setenv("A=B", "x", 1), errno == EINVAL);
    errno = 0;
    printf("bad-unset %d %d\n", unsetenv("A=B"), errno == EINVAL);

    char t1[] = "s32tmpXXXXXX", t2[] = "s32tmpXXXXXX";
    int f1 = mkstemp(t1), f2 = mkstemp(t2);
    printf("mkstemp %d %d\n", f1 >= 0, f2 >= 0);
    printf("names %d %d\n", strcmp(t1, "s32tmpXXXXXX") != 0, strcmp(t1, t2) != 0);
    printf("write %d\n", (int)write(f1, "abc", 3));
    close(f1);
    close(f2);
    FILE *r = fopen(t1, "r");
    char buf[8] = {0};
    printf("read %d %s\n", r ? (int)fread(buf, 1, 3, r) : -1, buf);
    if (r) fclose(r);
    printf("remove %d %d\n", remove(t1), remove(t2));

    char host[256];
    printf("hostname %d %d\n", gethostname(host, sizeof host), host[0] != 0);
    printf("ids %d\n", getuid() == geteuid() && getgid() == getegid());
    struct tms tm;
    printf("times %d\n", times(&tm) != (clock_t)-1);
    printf("clk-tck %d\n", sysconf(_SC_CLK_TCK) > 0);
    return 0;
}
