/* HOSTLEG -- getcwd over MMIO: the directory follows chdir, a buffer
 * too small for it gives NULL (ERANGE) rather than a cut-off path, and
 * the result is printed only from the last component on, since each
 * leg names its scratch directory its own way (/tmp vs /private/tmp). */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

static const char *tail(const char *p) {
    const char *t;
    t = strrchr(p, '/');
    return t ? t + 1 : p;
}

int main(void) {
    char buf[1024];
    char tiny[3];
    char *r;
    size_t n;
    r = getcwd(buf, sizeof buf);
    printf("getcwd returns buf: %d\n", r == buf);
    n = strlen(buf);
    printf("absolute: %d\n", buf[0] == '/');
    mkdir("heron_dir", 0755);
    printf("chdir in: %d\n", chdir("heron_dir"));
    r = getcwd(buf, sizeof buf);
    printf("now in: %s\n", r ? tail(r) : "(null)");
    printf("one level deeper: %d\n", r && strlen(r) == n + 10);
    printf("chdir out: %d\n", chdir(".."));
    r = getcwd(buf, sizeof buf);
    printf("back: %d\n", r && strlen(r) == n);
    printf("too small gives null: %d\n", getcwd(tiny, sizeof tiny) == NULL);
    printf("size 0 gives null: %d\n", getcwd(buf, 0) == NULL);
    return 0;
}
