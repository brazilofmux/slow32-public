/* 7.22.4.8 system: with a null pointer it only reports whether a command processor exists */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    int r = system(NULL);
    (void)r;
    printf("system(NULL) returned\n");
    return 0;
}
