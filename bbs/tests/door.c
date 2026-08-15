#include <stdio.h>

int main(void) {
    char buf[64];

    printf("FORTUNE\n");
    printf("Say something: ");
    fflush(stdout);
    if (!fgets(buf, (int)sizeof(buf), stdin)) {
        return 1;
    }
    printf("You said: %s", buf);
    fflush(stdout);
    return 0;
}
