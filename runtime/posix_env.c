/* What POSIX adds that a Fortran runtime, and much else, expects: setenv,
 * unsetenv, putenv, mkstemp, gethostname, times, and the user ids.  One source
 * for both libraries, as strerror.c is.
 *
 * The environment is the host's, asked for a name at a time, and a guest cannot
 * change that.  So a change is kept here, in entries of the form name=value, or
 * name alone for one unset, and getenv asks here first through __env_hook.  A
 * program that never changes the environment links none of this.  An answer
 * stays good after a later change: nothing here is freed that getenv handed
 * out, which is what glibc's setenv does and what programs written there count
 * on.
 *
 * A guest has no users and no network name of its own: the ids are 0, and the
 * host name is the machine's. */
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/times.h>
#include <time.h>
#include <unistd.h>

extern char *(*__env_hook)(const char *name, int *found);
extern char ***__env_vector;       /* &environ, in a program that names it */

static char **env_entry;
static int env_n, env_cap;

static int env_name_len(const char *entry) {
    const char *eq = strchr(entry, '=');
    return eq ? (int)(eq - entry) : (int)strlen(entry);
}

static int env_find(const char *name, int len) {
    for (int i = 0; i < env_n; i++)
        if (env_name_len(env_entry[i]) == len && memcmp(env_entry[i], name, (size_t)len) == 0)
            return i;
    return -1;
}

static char *env_lookup(const char *name, int *found) {
    int i = env_find(name, (int)strlen(name));
    if (i < 0) { *found = 0; return NULL; }
    *found = 1;
    char *eq = strchr(env_entry[i], '=');
    return eq ? eq + 1 : NULL;
}

static int env_name_ok(const char *name) {
    return name && *name && !strchr(name, '=');
}

/* environ as a new vector: what it held, less this name, plus the entry if it
 * has a value.  The old one is the host's or was seen by the program: kept. */
static int env_vector_update(char *entry, const char *name, int len) {
    if (!__env_vector) return 0;
    char **old = *__env_vector;
    int n = 0;
    while (old && old[n]) n++;
    char **v = (char **)malloc(sizeof(char *) * (size_t)(n + 2));
    if (!v) { errno = ENOMEM; return -1; }
    int k = 0;
    for (int i = 0; i < n; i++)
        if (!(strncmp(old[i], name, (size_t)len) == 0 && old[i][len] == '='))
            v[k++] = old[i];
    if (strchr(entry, '=')) v[k++] = entry;
    v[k] = NULL;
    *__env_vector = v;
    return 0;
}

static int env_store(char *entry, const char *name, int len) {
    int i = env_find(name, len);
    if (i < 0) {
        if (env_n == env_cap) {
            int cap = env_cap ? env_cap * 2 : 16;
            char **g = (char **)realloc(env_entry, sizeof(char *) * (size_t)cap);
            if (!g) { errno = ENOMEM; return -1; }
            env_entry = g;
            env_cap = cap;
        }
        i = env_n++;
    }
    env_entry[i] = entry;
    __env_hook = env_lookup;
    return env_vector_update(entry, name, len);
}

int setenv(const char *name, const char *value, int overwrite) {
    if (!env_name_ok(name) || !value) { errno = EINVAL; return -1; }
    if (!overwrite && getenv(name)) return 0;
    size_t nl = strlen(name), vl = strlen(value);
    char *entry = (char *)malloc(nl + vl + 2);
    if (!entry) { errno = ENOMEM; return -1; }
    memcpy(entry, name, nl);
    entry[nl] = '=';
    memcpy(entry + nl + 1, value, vl + 1);
    return env_store(entry, name, (int)nl);
}

int unsetenv(const char *name) {
    if (!env_name_ok(name)) { errno = EINVAL; return -1; }
    size_t nl = strlen(name);
    char *entry = (char *)malloc(nl + 1);
    if (!entry) { errno = ENOMEM; return -1; }
    memcpy(entry, name, nl + 1);
    return env_store(entry, name, (int)nl);
}

/* The string itself becomes the entry: changing it changes the environment. */
int putenv(char *string) {
    if (!string || !*string || *string == '=' || !strchr(string, '=')) { errno = EINVAL; return -1; }
    return env_store(string, string, env_name_len(string));
}

int mkstemp(char *tmpl) {
    static const char letters[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    static unsigned long counter;
    size_t len = tmpl ? strlen(tmpl) : 0;
    if (len < 6 || strcmp(tmpl + len - 6, "XXXXXX") != 0) { errno = EINVAL; return -1; }
    struct timespec ts;
    unsigned long seed = 0;
    if (clock_gettime(CLOCK_REALTIME, &ts) == 0)
        seed = (unsigned long)ts.tv_nsec ^ ((unsigned long)ts.tv_sec << 7);
    for (int tries = 0; tries < 10000; tries++) {
        unsigned long v = seed + (unsigned long)getpid() * 31UL + ++counter * 2654435761UL;
        for (int k = 1; k <= 6; k++) {
            tmpl[len - (size_t)k] = letters[v % 62UL];
            v /= 62UL;
        }
        int fd = open(tmpl, O_RDWR | O_CREAT | O_EXCL, 0600);
        if (fd >= 0) return fd;
        if (errno != EEXIST) return -1;
    }
    errno = EEXIST;
    return -1;
}

int gethostname(char *name, size_t len) {
    static const char host[] = "slow32";
    if (!name) { errno = EFAULT; return -1; }
    if (len < sizeof host) { errno = ENAMETOOLONG; return -1; }
    memcpy(name, host, sizeof host);
    return 0;
}

uid_t getuid(void) { return 0; }
uid_t geteuid(void) { return 0; }
gid_t getgid(void) { return 0; }
gid_t getegid(void) { return 0; }

/* In ticks of _SC_CLK_TCK.  No children and no system time of its own. */
clock_t times(struct tms *t) {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return (clock_t)-1;
    if (t) {
        clock_t c = clock();
        t->tms_utime = c == (clock_t)-1 ? 0 : c / (CLOCKS_PER_SEC / SLOW32_CLK_TCK);
        t->tms_stime = 0;
        t->tms_cutime = 0;
        t->tms_cstime = 0;
    }
    return (clock_t)(ts.tv_sec * SLOW32_CLK_TCK + ts.tv_nsec / (1000000000L / SLOW32_CLK_TCK));
}
