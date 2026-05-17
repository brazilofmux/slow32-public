static int split2(char *s, char **t0, char **t1) {
    int n = 0;
    *t0 = 0;
    *t1 = 0;
    while (*s && n < 2) {
        while (*s == ' ' || *s == '\t' || *s == ',') s++;
        if (*s == 0) break;
        if (n == 0) *t0 = s;
        else *t1 = s;
        n++;
        while (*s && *s != ' ' && *s != '\t' && *s != ',') s++;
        if (*s == 0) break;
        *s = 0;
        s++;
    }
    return n;
}

int main(void) {
    char line[] = "addi r29";
    char *a;
    char *b;
    if (split2(line, &a, &b) != 2) return 1;
    if (a[0] != 'a' || a[1] != 'd' || a[2] != 'd' || a[3] != 'i' || a[4] != 0) return 2;
    if (b[0] != 'r' || b[1] != '2' || b[2] != '9' || b[3] != 0) return 3;
    return 0;
}
