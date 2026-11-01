/* 7.21.8.1-2 fread fwrite: return the number of whole elements transferred; a short read leaves feof set; size or count 0 returns 0 */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("rw.bin", "wb+");
    if (!f) { printf("fopen failed\n"); return 1; }
    int v[5] = { 1, 2, 3, 4, 5 }, w[5] = { 0 };
    printf("%zu %zu %zu\n", fwrite(v, sizeof v[0], 5, f), fwrite(v, 0, 5, f), fwrite(v, 4, 0, f));
    fputc('x', f);
    rewind(f);
    printf("%zu %d %d\n", fread(w, sizeof w[0], 3, f), w[0], w[2]);
    printf("%zu %d %d\n", fread(w, sizeof w[0], 3, f), w[0], w[1]);
    printf("%d %d\n", feof(f) != 0, ferror(f) != 0);
    printf("%zu\n", fread(w, 1, 0, f));
    rewind(f);
    char b[32];
    printf("%zu %d\n", fread(b, 1, sizeof b, f), b[20]);
    fclose(f);
    remove("rw.bin");
    return 0;
}
