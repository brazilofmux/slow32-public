static int str_eq(const char *a, const char *b) {
    int i = 0;
    for (;;) {
        if (a[i] != b[i]) return 0;
        if (a[i] == 0) return 1;
        i++;
    }
}

int main(void) {
    if (!str_eq("addi", "addi")) return 1;
    if (str_eq("addi", "add")) return 2;
    if (str_eq("add", "addi")) return 3;
    return 0;
}
