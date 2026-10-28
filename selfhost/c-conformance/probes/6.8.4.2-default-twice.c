// 6.8.4.2p3  at most one default label in a switch statement
// expect: reject
int main(void) {
    int x = 1;
    switch (x) { default: x = 2; break; case 3: default: x = 4; }
    return x;
}
