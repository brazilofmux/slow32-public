/* 7.14.2.1 raise: a number that is no signal cannot be raised successfully, so raise returns nonzero and the program goes on */
#include <stdio.h>
#include <signal.h>

int main(void) {
    printf("%d %d\n", raise(-1) != 0, raise(9999) != 0);
    return 0;
}
