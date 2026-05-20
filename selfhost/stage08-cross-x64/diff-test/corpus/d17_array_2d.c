/* Indexing patterns that look 2D but use 1D arrays under the hood
 * (cc-x64 doesn't accept `int g[N][N];` declarators).  Each access
 * has a computed offset like `i * N + j` so the compiler must lower
 * a multiplication into the index — stresses SIB folds with non-
 * trivial scale/offset combinations.  Three element widths cover
 * scale=4 (int), scale=2 (short), scale=1 (char). */
#define N 12

static int           g_int[N * N];
static unsigned short g_short[N * N];
static unsigned char  g_char[N * N];

static int seed(int i, int j) {
    return ((i * 7919) ^ (j * 31)) + (i * j) - i + j;
}

int main(void) {
    int i, j;
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            int s = seed(i, j);
            g_int[i * N + j]   = s;
            g_short[i * N + j] = (unsigned short)(s & 0xFFFF);
            g_char[i * N + j]  = (unsigned char)(s & 0xFF);
        }
    }

    int acc = 0;
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            int idx_a = ((i + j) % N) * N + ((i ^ j) % N);
            int idx_b = ((i ^ j) % N) * N + ((i + j) % N);
            acc = acc * 5 + g_int[idx_a];
            acc = acc + (int)g_short[idx_b];
            acc = acc - (int)g_char[idx_a];
        }
    }
    return acc & 0xff;
}
