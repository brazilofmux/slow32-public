/* 7.21.5.3 fopen: in append modes every write goes to the end of file regardless of fseek */
#include <stdio.h>

static void show(const char *name) {
    FILE *f = fopen(name, "rb");
    int c;
    putchar('[');
    while ((c = fgetc(f)) != EOF) putchar(c);
    printf("]\n");
    fclose(f);
}

int main(void) {
    FILE *f = fopen("ap.txt", "w");
    fputs("base", f);
    fclose(f);
    f = fopen("ap.txt", "a");
    fputs("-one", f);
    fseek(f, 0, SEEK_SET);
    fputs("-two", f);
    fclose(f);
    show("ap.txt");
    f = fopen("ap.txt", "a+");
    rewind(f);
    printf("%c", fgetc(f));
    fseek(f, 0, SEEK_SET);
    fputs("-three", f);
    fflush(f);
    fseek(f, 0, SEEK_SET);
    char b[64] = "";
    fgets(b, sizeof b, f);
    printf(" [%s]\n", b);
    fclose(f);
    f = fopen("new-ap.txt", "a");
    printf("%d\n", f != NULL);
    fputs("created", f);
    fclose(f);
    show("new-ap.txt");
    remove("ap.txt");
    remove("new-ap.txt");
    return 0;
}
