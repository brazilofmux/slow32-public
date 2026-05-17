static int has_flag(const char *cmd, char ch) {
    while (*cmd) {
        if (*cmd == ch) return 1;
        cmd = cmd + 1;
    }
    return 0;
}

static const char *basename_ptr(const char *path) {
    const char *p = path;
    const char *base = path;
    while (*p) {
        if (*p == '/' || *p == '\\') base = p + 1;
        p = p + 1;
    }
    return base;
}

static int streq(const char *a, const char *b) {
    while (*a && *b) {
        if (*a != *b) return 0;
        a = a + 1;
        b = b + 1;
    }
    return *a == *b;
}

static int name_matches(const char *member_name, const char *target_name) {
    if (streq(member_name, target_name)) return 1;
    return streq(basename_ptr(member_name), target_name);
}

static int delete_one(int *vals, int n, int target) {
    int i;
    int out_n = 0;
    int found = 0;
    for (i = 0; i < n; i = i + 1) {
        int remove = vals[i] == target;
        if (remove) found = 1;
        if (!remove) {
            vals[out_n] = vals[i];
            out_n = out_n + 1;
        }
    }
    if (!found) return -1;
    return out_n;
}

static int dispatch_delete(const char *cmd, int *vals, int n, int target) {
    if (has_flag(cmd, 't')) return n;
    if (has_flag(cmd, 'x')) return n;
    if (has_flag(cmd, 'd')) return delete_one(vals, n, target);
    if (has_flag(cmd, 'r')) return n;
    if (has_flag(cmd, 'c')) return n;
    return -1;
}

int main(void) {
    int vals[8];
    int n;

    vals[0] = 4;
    vals[1] = 7;
    vals[2] = 9;
    vals[3] = 11;
    vals[4] = 9;
    vals[5] = 13;
    vals[6] = 17;
    vals[7] = 0;

    if (!name_matches("tmp/member-a.src", "member-a.src")) return 1;
    if (name_matches("tmp/member-a.src", "member-b.src")) return 2;

    n = dispatch_delete("rcd", vals, 7, 9);
    if (n != 5) return 3;
    if (vals[0] != 4) return 4;
    if (vals[1] != 7) return 5;
    if (vals[2] != 11) return 6;
    if (vals[3] != 13) return 7;
    if (vals[4] != 17) return 8;
    return 0;
}
