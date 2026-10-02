/* A device that refuses a write (stdio_fault.fault: the emulator fails
 * the first, third and fifth write requests to a file with ENOSPC).
 *
 * Buffered output finds out when a buffer is sent, and the call that
 * sends it must say so: its bytes were in the buffer that was refused.
 * The clang runtime's fwrite counted them as written when they filled
 * the buffer exactly -- so a program writing one byte at a time lost a
 * buffer of 4,096 and every call returned 1 (runtime ISSUES-28).
 *
 * Each part: how many calls failed and which was the first, the
 * stream's error indicator, what fclose says of the rest (the next
 * request, which succeeds), and how many bytes reached the file. */
#include <stdio.h>

static long size_of(const char *name)
{
    FILE *f = fopen(name, "rb");
    long n = 0;
    if (!f) return -1;
    while (fgetc(f) != EOF) n++;
    fclose(f);
    return n;
}

int main(void)
{
    FILE *f;
    int i, bad, first;
    char rec[100];

    f = fopen("fault1.dat", "wb");
    bad = 0; first = 0;
    for (i = 1; i <= 5000; i++) {
        char c = 'x';
        if (fwrite(&c, 1, 1, f) != 1) { bad++; if (!first) first = i; }
    }
    printf("fwrite of one byte: %d failed, the first the %dth; error %d\n", bad, first, ferror(f) != 0);
    printf("fclose %d, in the file %ld\n", fclose(f), size_of("fault1.dat"));

    f = fopen("fault2.dat", "wb");
    bad = 0; first = 0;
    for (i = 1; i <= 5000; i++)
        if (fputc('y', f) == EOF) { bad++; if (!first) first = i; }
    printf("fputc: %d failed, the first the %dth; error %d\n", bad, first, ferror(f) != 0);
    printf("fclose %d, in the file %ld\n", fclose(f), size_of("fault2.dat"));

    for (i = 0; i < 100; i++) rec[i] = 'z';
    f = fopen("fault3.dat", "wb");
    bad = 0; first = 0;
    for (i = 1; i <= 60; i++)
        if (fwrite(rec, 100, 1, f) != 1) { bad++; if (!first) first = i; }
    printf("fwrite of a hundred: %d failed, the first the %dth; error %d\n", bad, first, ferror(f) != 0);
    printf("fclose %d, in the file %ld\n", fclose(f), size_of("fault3.dat"));
    return 0;
}
