// 6.8.4.2p2  a case label inside the scope of a VLA requires the whole switch to be in that scope
// expect: reject
int main(int argc, char **argv) {
    (void)argv;
    switch (argc) {
    case 0: ;
        int a[argc + 1];
        a[0] = 1;
    case 1:
        return 0;
    }
    return 1;
}
