/* 7.21.5.6 setvbuf: _IOFBF _IOLBF _IONBF with and without a caller buffer; no output is lost; an invalid mode is refused */
#include <stdio.h>

static char big[1024];

static void show(const char *name) {
    FILE *f = fopen(name, "r");
    int c, n = 0;
    while ((c = fgetc(f)) != EOF) n++;
    fclose(f);
    printf("%d ", n);
}

int main(void) {
    int modes[3] = { _IOFBF, _IOLBF, _IONBF };
    for (int m = 0; m < 3; m++) {
        for (int own = 0; own < 2; own++) {
            FILE *f = fopen("vb.txt", "w");
            int r = setvbuf(f, own ? big : NULL, modes[m], own ? sizeof big : 64);
            for (int i = 0; i < 300; i++) fprintf(f, "%d\n", i % 10);
            fclose(f);
            printf("%d:", r);
            show("vb.txt");
        }
    }
    printf("\n");
    FILE *f = fopen("vb.txt", "w");
    printf("%d\n", setvbuf(f, NULL, 12345, 0) != 0);
    fclose(f);
    f = fopen("vb.txt", "w");
    setbuf(f, NULL);
    fputs("unbuffered", f);
    FILE *g = fopen("vb.txt", "r");
    char b[16] = "";
    fgets(b, sizeof b, g);
    printf("[%s]\n", b);
    fclose(g);
    fclose(f);
    f = fopen("vb.txt", "w");
    setbuf(f, big);
    fputs("with BUFSIZ buffer", f);
    fclose(f);
    show("vb.txt");
    printf("\n");
    remove("vb.txt");
    return 0;
}
