/* 7.21.4.1-2 remove rename: return zero on success and nonzero on failure; after rename the old name is gone */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("old.txt", "w");
    fputs("content", f);
    fclose(f);
    remove("new.txt");
    printf("%d\n", rename("old.txt", "new.txt") == 0);
    printf("%d\n", fopen("old.txt", "r") == NULL);
    f = fopen("new.txt", "r");
    char b[16] = "";
    if (f) { fgets(b, sizeof b, f); fclose(f); }
    printf("[%s]\n", b);
    printf("%d\n", rename("old.txt", "other.txt") != 0);
    printf("%d\n", remove("new.txt") == 0);
    printf("%d\n", fopen("new.txt", "r") == NULL);
    printf("%d\n", remove("new.txt") != 0);
    return 0;
}
