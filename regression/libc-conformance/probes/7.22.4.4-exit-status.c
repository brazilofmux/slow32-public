/* 7.22.4.4 exit: EXIT_FAILURE gives an unsuccessful status; the value given is the status */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    printf("EXIT_SUCCESS==0: %d differ: %d\n", EXIT_SUCCESS == 0, EXIT_FAILURE != EXIT_SUCCESS);
    exit(EXIT_FAILURE);
}
