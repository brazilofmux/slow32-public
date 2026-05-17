static int has_flag(const char *cmd, char ch) {
    while (*cmd) {
        if (*cmd == ch) return 1;
        cmd++;
    }
    return 0;
}

int main(void) {
    if (!has_flag("rc", 'r')) return 1;
    if (!has_flag("rc", 'c')) return 2;
    if (has_flag("rc", 'x')) return 3;
    return 0;
}
