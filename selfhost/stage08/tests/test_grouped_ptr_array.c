/* GitHub issue 47: char (*a[]) is char *a[], not an array of
 * function pointers.  The grouping hack only recognised the star
 * outside: char *(azHelp[]).  Own file because test_sqlite_bugs.c's
 * t_grouped_declarator returns 1024, which hides as process rc 0. */
static const char *(star_out[]) = { "out" };
static const char (*star_in[]) = { "in" };

int main(void) {
    const char *(local_out[]) = { "lo" };
    const char (*local_in[]) = { "li" };
    if (star_out[0][0] != 'o' || star_out[0][1] != 'u') return 1;
    if (star_in[0][0] != 'i' || star_in[0][1] != 'n') return 2;
    if (local_out[0][0] != 'l' || local_out[0][1] != 'o') return 3;
    if (local_in[0][0] != 'l' || local_in[0][1] != 'i') return 4;
    if (sizeof(star_in) / sizeof(star_in[0]) != 1) return 5;
    return 0;
}
