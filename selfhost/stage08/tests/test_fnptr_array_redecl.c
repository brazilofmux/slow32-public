/* An array of function pointers declared, then defined: the
 * redeclaration check compared the definition against a type left over
 * from the previous declaration and called the two "a different type"
 * (6.7p4 has them compatible: [] completes to [2]).  s32-xbc declares
 * its block table before the code that uses it and defines it after,
 * and the use, made while the array's size was unknown, loaded the
 * table's first word as its address. */
static int f1(int x) { return x + 1; }
static int f2(int x) { return x + 2; }

extern int (*const tab[])(int);
static int use_early(int i, int x) { return tab[i](x); }   /* size still unknown */
int (*const tab[])(int) = { f1, f2 };

extern int (*tab2[2])(int);
int (*tab2[2])(int);

int main(void) {
    if (tab[0](1) != 2 || tab[1](1) != 3) return 1;
    if (use_early(1, 10) != 12 || use_early(0, 10) != 11) return 3;
    tab2[1] = f2;
    if (tab2[1](5) != 7) return 2;
    return 0;
}
