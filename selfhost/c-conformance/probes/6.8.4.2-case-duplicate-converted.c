// 6.8.4.2p3  case values are compared after conversion to the promoted controlling type
// expect: reject
int main(void) {
    unsigned int x = 1;
    switch (x) { case 0xFFFFFFFFu: return 1; case -1: return 2; }
    return 0;
}
