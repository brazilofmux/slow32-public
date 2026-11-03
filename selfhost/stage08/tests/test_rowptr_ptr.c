/* A pointer to a pointer to an array, T (**p)[N]: as a parameter it was
 * typed as one pointer, so *p was the row itself, an array, and
 * `*p = calloc(...)` "not assignable" (sbasic's name_slot); as a local
 * it was refused. */
void *calloc(unsigned long n, unsigned long size);

static char *slot(char (**list)[64], int cap, int n) {
    if (n >= cap) return 0;
    if (!*list && !(*list = calloc((unsigned long)cap, sizeof **list))) return 0;
    return (*list)[n];
}

static void set(int (**pp)[3], int (*v)[3]) { *pp = v; }

int rows[4][3];

int main(void) {
    char (*names)[64] = 0;
    char *a;
    char *b;
    int (*r)[3] = 0;
    int (**rr)[3] = &r;
    a = slot(&names, 8, 2);
    b = slot(&names, 8, 3);
    if (!a || !names) return 1;
    if (b - a != 64) return 2;                         /* rows of 64 */
    if (sizeof *&names != sizeof(void *) || sizeof *names != 64) return 3;
    set(&r, rows + 1);
    if (r != rows + 1) return 4;
    (*rr)[1][2] = 42;                                  /* rows[2][2] */
    if (rows[2][2] != 42) return 5;
    if ((char *)(*rr + 1) - (char *)*rr != 12) return 6;
    if (sizeof *rr != sizeof(void *) || sizeof **rr != 12) return 7;
    return 0;
}
