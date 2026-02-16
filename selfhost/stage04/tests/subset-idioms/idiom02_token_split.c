static int split3(char *s, char **t0, char **t1, char **t2) {
    int n = 0;
    *t0 = 0; *t1 = 0; *t2 = 0;
    while (*s && n < 3) {
        while (*s == ' ' || *s == '\t' || *s == ',') s++;
        if (*s == 0) break;
        if (n == 0) *t0 = s;
        else if (n == 1) *t1 = s;
        else *t2 = s;
        n++;
        while (*s && *s != ' ' && *s != '\t' && *s != ',') s++;
        if (*s == 0) break;
        *s = 0;
        s++;
    }
    return n;
}

int main(void) {
    char line[] = "x arc.s32a member.o";
    char *a, *b, *c;
    if (split3(line, &a, &b, &c) != 3) return 1;
    if (a[0] != 'x' || a[1] != 0) return 2;
    if (b[0] != 'a') return 3;
    if (c[0] != 'm') return 4;
    return 0;
}
