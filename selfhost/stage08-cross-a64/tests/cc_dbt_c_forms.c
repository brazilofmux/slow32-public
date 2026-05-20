typedef unsigned long long uint64_t;

typedef struct {
    const char *name;
    int value;
    void *handler;
} entry_t;

static int target(int x) {
    return x + 1;
}

static const entry_t entries[] = {
    {
        .name = "term",
        .value = 14,
        .handler = target,
    },
};

static int scan_names(void) {
    static const char hex[] = "0123456789ABCDEF";
    static const char *names[] = {
        "memcpy",
        "memset",
        0,
    };
    int sum;
    static char scratch[2][8];

    scratch[0] = 'x';
    sum = hex[10];
    for (char *const *p = names; *p != 0; ++p) {
        sum = sum + (*p)[3];
    }
    return sum + scratch[0];
}

int main(void) {
    (void)entries;
    (void)scan_names;
    return 1;
}
