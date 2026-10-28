// 6.5.3.4p2/6.7.6.2p4  sizeof a VLA is evaluated at run time
#include <stdio.h>
static unsigned sz(int n) { char a[n + 1]; return (unsigned)sizeof a; }
int main(void) { printf("%u %u\n", sz(3), sz(40)); return 0; }
