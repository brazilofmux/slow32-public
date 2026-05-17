int main(void) {
    char s[] = "a b";
    char *p = s;
    int n = 0;
    while (*p && *p != ' ' && *p != '\t' && *p != ',') {
        n++;
        p++;
    }
    if (n != 1) return 1;
    if (*p != ' ') return 2;
    return 0;
}
