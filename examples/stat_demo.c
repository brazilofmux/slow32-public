#include <stdio.h>
#include <sys/stat.h>

static void dump_stat(const char *label, const struct stat *st) {
    printf("%s: size=%llu bytes mode=%o mtime=%llu.%09u\n",
           label,
           (unsigned long long)st->st_size,
           st->st_mode,
           (unsigned long long)st->st_mtime,
           st->st_mtime_nsec);
}

int main(void) {
    const char *path = "slow32cc";
    struct stat info = {0};

    if (stat(path, &info) != 0) {
        printf("stat(%s) failed\n", path);
        return 1;
    }
    dump_stat("stat", &info);

    struct stat stdout_info = {0};
    if (fstat(1, &stdout_info) != 0) {
        printf("fstat(1) failed\n");
        return 1;
    }
    dump_stat("stdout", &stdout_info);

    puts("stat demo complete");
    return 0;
}
