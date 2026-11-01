/* 7.21.9.5 rewind: sets the position to the start and clears the error and end-of-file indicators */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("rew.txt", "w+");
    fputs("ab", f);
    fseek(f, 0, SEEK_SET);
    while (fgetc(f) != EOF) ;
    printf("%d ", feof(f) != 0);
    rewind(f);
    printf("%d %ld %c\n", feof(f) != 0, ftell(f), fgetc(f));
    fclose(f);
    remove("rew.txt");
    return 0;
}
