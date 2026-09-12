/* A parameter declared `T a[N][M]` is a pointer to a row of M elements:
 * one pointer level, and a[i] selects row i without loading through it.
 * It used to become `T **`, and regal's CSV row callback passed the
 * first row's BYTES to strcasecmp as a pointer (memory fault at
 * "Clea" of "Cleared").  The `T (*p)[M]` spelling of the same type is
 * still not parsed in a parameter list. */
#include <string.h>
#define NF 4
#define FL 16
static int row_cb(int n, char fields[NF][FL], int *hits) {
    int i;
    if (n < 2) return 0;
    if (strcmp(fields[0], "Cleared") != 0) return 1;
    for (i = 1; i < n; i++) if (fields[i][0] != '\0') *hits += (int)strlen(fields[i]);
    if (sizeof(fields[1]) != FL) return 2;
    return 0;
}
int main(void) {
    char f[NF][FL];
    int hits = 0;
    strcpy(f[0], "Cleared"); strcpy(f[1], "2025-10-03"); strcpy(f[2], ""); strcpy(f[3], "12.34");
    if (row_cb(4, f, &hits) != 0) return 10;
    if (hits != 15) return 20;
    strcpy(f[0], "Pending");
    if (row_cb(4, f, &hits) != 1) return 40;
    return 0;
}
