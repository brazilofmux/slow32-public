// 6.8.1p1  in C11 a label prefixes a statement; a label directly before a declaration is a syntax error
// expect: reject
int main(void) {
    int n = 1;
    goto here;
here:
    int k = n;
    return k;
}
