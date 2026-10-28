// 6.10.2p1  #include must name a file the implementation can find
// expect: reject
#include "inc/6.10.2-no-such-file.h"
int main(void) { return 0; }
