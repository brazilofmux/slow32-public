int count_len(char *s) {
    int n;
    n = 0;
    while (s[n] != 0) n = n + 1;
    return n;
}

int main(void) {
    char buf[8];
    buf[0] = 104;
    buf[1] = 101;
    buf[2] = 108;
    buf[3] = 108;
    buf[4] = 111;
    buf[5] = 33;
    buf[6] = 0;
    return count_len(buf);
}
