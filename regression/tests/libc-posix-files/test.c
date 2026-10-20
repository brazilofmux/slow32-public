/* The POSIX file calls std::filesystem needs, from C: truncate, realpath,
 * remove of a directory, readlink of a plain file, pathconf, chmod to the
 * mode a file has, fseeko/ftello.  Works in results/libc-posix-files/;
 * prints only what does not depend on where that is (expected.txt is a
 * host build's output). */
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#define DIR_ "results/libc-posix-files/work"

static int ends_with(const char *s, const char *t) {
    size_t a = strlen(s), b = strlen(t);
    return a >= b && strcmp(s + a - b, t) == 0;
}

int main(void) {
    struct stat st;
    remove(DIR_ "/f.txt");
    remove(DIR_);
    printf("mkdir: %d\n", mkdir(DIR_, 0755));
    FILE *f = fopen(DIR_ "/f.txt", "w");
    fputs("abcdefghij", f);
    fclose(f);

    printf("truncate: %d\n", truncate(DIR_ "/f.txt", 3));
    stat(DIR_ "/f.txt", &st);
    printf("size after: %lld\n", (long long)st.st_size);

    char buf[PATH_MAX];
    char *r = realpath(DIR_ "/./../work//f.txt", buf);
    printf("realpath: %s absolute=%d tail=%d\n", r ? "ok" : "null", r && r[0] == '/',
           r && ends_with(r, "/results/libc-posix-files/work/f.txt"));
    errno = 0;
    r = realpath(DIR_ "/missing.txt", buf);
    printf("realpath missing: %s enoent=%d\n", r ? "ok" : "null", errno == ENOENT);
    char *m = realpath(".", NULL);
    printf("realpath malloc: %d\n", m && m[0] == '/');
    free(m);

    errno = 0;
    printf("readlink file: %ld einval=%d\n", (long)readlink(DIR_ "/f.txt", buf, sizeof buf), errno == EINVAL);
    printf("pathconf: %d %d\n", pathconf(".", _PC_PATH_MAX) >= 256, pathconf(".", _PC_NAME_MAX) >= 14);
    stat(DIR_ "/f.txt", &st);
    printf("chmod same mode: %d\n", chmod(DIR_ "/f.txt", st.st_mode & 07777));

    f = fopen(DIR_ "/f.txt", "r");
    int sk = fseeko(f, 2, SEEK_SET);
    long long at = (long long)ftello(f);
    int ch = getc(f);
    printf("fseeko: %d ftello: %lld getc: %c\n", sk, at, ch);
    fclose(f);

    errno = 0;
    printf("remove full dir: %d\n", remove(DIR_) == 0);
    printf("remove file: %d\n", remove(DIR_ "/f.txt"));
    printf("remove dir: %d\n", remove(DIR_));
    printf("gone: %d\n", stat(DIR_, &st) != 0 && errno == ENOENT);
    return 0;
}
