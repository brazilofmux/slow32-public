// 6.7.6.2p4/6.9.2p2  an incomplete array declaration completed by a later definition
#include <stdio.h>
extern int ext[];
int ext[4] = { 1, 2 };
int main(void) { printf("%d %u\n", ext[1] + ext[3], (unsigned)sizeof ext); return 0; }
