/* 7.22.4.6 getenv: a name not in the environment list gives a null pointer */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    printf("%d %d %d\n", getenv("SLOW32_PROBE_NOT_SET_ANYWHERE") == NULL, getenv("") == NULL,
           getenv("SLOW32_PROBE_NOT_SET_ANYWHERE=x") == NULL);
    return 0;
}
