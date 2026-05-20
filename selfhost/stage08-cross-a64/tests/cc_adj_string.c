#define MID "b"

int main(void) {
    char *s;
    s = "a" MID "c";
    if (s[0] != 'a') return 2;
    if (s[1] != 'b') return 3;
    if (s[2] != 'c') return 4;
    if (s[3] != 0) return 5;
    return 1;
}
