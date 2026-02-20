int main(void) {
    int r;
    r = 0;
    if ('A' == 65) r = r + 1;
    if ('\n' == 10) r = r + 1;
    if ('\\' == 92) r = r + 1;
    return r;
}
