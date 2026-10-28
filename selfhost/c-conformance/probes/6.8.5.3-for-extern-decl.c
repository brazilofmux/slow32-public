// 6.8.5p3  an extern declaration in a for clause-1 is not allowed
// expect: reject
int g;
int main(void) {
    for (extern int g; g < 3; g++) ;
    return g;
}
