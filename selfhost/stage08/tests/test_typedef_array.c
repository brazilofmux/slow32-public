/* selfhost ISSUES-75: an array of a typedef'd array type.
 *
 * A typedef of an array type is spelled, inside the compiler, as a
 * pointer to the element with the dimension on the side, and every
 * declarator but the plain one -- `row_t one;` -- lost the dimension:
 * `row_t rows[4]` was four pointers (16 bytes for 336), a function's
 * `static row_t r;` was one pointer, sizeof(row_t) was 4, and
 * sizeof rows[0] of any two-dimensional array was 4.  Nothing said so;
 * setjmp(handlers[n]) stored through a null pointer.
 *
 * Returns 0, or the number of the first check that fails. */
typedef int row_t[21];
typedef char name_t[8];

static row_t rows[4];
row_t pub[2] = {{1, 2, 3}, {4, 5, 6}};
static row_t one;
name_t names[3] = {"alpha", "beta", "gamma"};
int plain[4][21];

struct holder { int tag; row_t r; row_t rr[3]; name_t nm[2]; int tail; };
static struct holder h;
static int n;

static int dist(int *p, int *base) { return (int)((char *)p - (char *)base); }
static int sum(row_t r) { int i; int t; t = 0; for (i = 0; i < 21; i++) t += r[i]; return t; }
static int same(char *a, char *b) { while (*a && *a == *b) { a++; b++; } return *a == *b; }

int main(void) {
    row_t local[3];
    row_t la, lb;
    row_t li = {7, 8, 9};
    static row_t srow;
    static row_t stab[2];
    name_t ln[2];
    int i;
    int j;

    if (sizeof rows != 336) return 1;
    if (sizeof rows[0] != 84) return 2;
    if (sizeof one != 84) return 3;
    if (sizeof local != 252) return 4;
    if (sizeof la != 84 || sizeof lb != 84 || sizeof li != 84) return 5;
    if (sizeof srow != 84) return 6;
    if (sizeof stab != 168) return 7;
    if (sizeof names != 24) return 8;
    if (sizeof(struct holder) != 4 + 84 + 252 + 16 + 4) return 9;
    if (sizeof h.rr != 252 || sizeof h.rr[0] != 84 || sizeof h.nm != 16) return 10;
    if (sizeof ln != 16) return 11;
    if (sizeof(row_t) != 84 || sizeof(name_t) != 8) return 12;
    if (sizeof(int[5]) != 20 || sizeof(row_t[3]) != 252) return 13;
    if (sizeof plain[1] != 84 || sizeof local[2] != 84) return 14;

    if (dist(rows[2], rows[0]) != 168) return 20;
    if (dist(rows[n++], rows[0]) != 0) return 21;
    if (dist(rows[n++], rows[0]) != 84) return 22;
    if (dist(&rows[3][20], rows[0]) != 332) return 23;
    if (dist(local[2], local[0]) != 168) return 24;
    if (dist(stab[1], stab[0]) != 84) return 25;
    if (dist(h.rr[2], h.rr[0]) != 168) return 26;
    if (dist(h.rr[1], h.r) != 168) return 27;
    if ((int)((char *)&h.tail - (char *)&h) != 356) return 28;

    for (i = 0; i < 4; i++) for (j = 0; j < 21; j++) rows[i][j] = i * 100 + j;
    for (i = 0; i < 3; i++) for (j = 0; j < 21; j++) { local[i][j] = i + j; h.rr[i][j] = i * j; }
    for (j = 0; j < 21; j++) { la[j] = j; lb[j] = 2 * j; srow[j] = 3; stab[0][j] = 4; stab[1][j] = 5; h.r[j] = 6; one[j] = 1; }
    if (sum(rows[0]) != 210 || sum(rows[1]) != 2310 || sum(rows[3]) != 6510) return 30;
    if (sum(local[0]) != 210 || sum(local[2]) != 252) return 31;
    if (sum(la) != 210 || sum(lb) != 420) return 32;
    if (sum(srow) != 63 || sum(stab[0]) != 84 || sum(stab[1]) != 105) return 33;
    if (sum(h.r) != 126 || sum(h.rr[0]) != 0 || sum(h.rr[2]) != 420) return 34;
    if (sum(one) != 21) return 35;
    if (h.tag != 0 || h.tail != 0) return 36;       /* nothing ran past its array */

    if (pub[0][0] != 1 || pub[0][2] != 3 || pub[0][3] != 0 || pub[1][0] != 4 || pub[1][2] != 6 || pub[1][20] != 0) return 40;
    if (li[0] != 7 || li[2] != 9 || li[3] != 0 || li[20] != 0) return 41;
    if (!same(names[0], "alpha") || !same(names[1], "beta") || !same(names[2], "gamma")) return 42;
    ln[0][0] = 'a'; ln[0][1] = 0; ln[1][0] = 'b'; ln[1][1] = 0;
    if (!same(ln[0], "a") || !same(ln[1], "b")) return 43;
    return 0;
}
