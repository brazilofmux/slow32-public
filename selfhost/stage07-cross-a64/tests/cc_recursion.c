/* Tests recursive function calls. fact(7) = 5040, low byte = 5040 % 256 = 176.
 * exit() takes 8 bits on Linux, so we sum 5040 -> 5040 % 256 -> 176... but
 * 5040 % 256 = 176? 5040 = 19*256 + 176. So 176. We'll target a smaller
 * value: fact(5) = 120 fits in 8 bits. */

int fact(int n) {
    if (n <= 1) return 1;
    return n * fact(n - 1);
}

int main(void) {
    return fact(5);
}
