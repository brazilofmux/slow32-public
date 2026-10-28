// 6.8.6.1p1  goto shall not jump into the scope of a variably modified identifier
// expect: reject
int main(int argc, char **argv) {
    (void)argv;
    goto in;
    {
        int a[argc + 1];
    in:
        a[0] = 0;
        return a[0];
    }
}
