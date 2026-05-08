/* Arithmetic + cross-function call. Tests that constants and
   call-result values are placed and consumed correctly. */
int add(int a, int b) {
    return a + b;
}

int mul(int a, int b) {
    return a * b;
}

int main(void) {
    return add(3, 4) - mul(2, 7);
}
