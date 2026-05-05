/* Tests local variables, basic arithmetic, and parameter passing. */

int add(int a, int b) {
    return a + b;
}

int main(void) {
    int x;
    int y;
    x = 17;
    y = 25;
    return add(x, y);   /* expect 42 */
}
