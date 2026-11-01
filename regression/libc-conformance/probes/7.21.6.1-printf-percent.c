/* 7.21.6.1 fprintf: %% writes a single percent sign; ordinary characters are copied unchanged */
#include <stdio.h>

int main(void) {
    char b[32];
    int r = snprintf(b, sizeof b, "100%% %d%%", 5);
    printf("%d [%s]\n", r, b);
    r = printf("%%%%%%\n");
    printf("%d\n", r);
    printf("tab\there \\ \"q\" 'a' ?\n");
    return 0;
}
