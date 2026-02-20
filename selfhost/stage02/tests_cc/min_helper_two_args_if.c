int helper(int a, int b) {
    if (a < b) return a;
    return b;
}

int main(void) {
    return helper(9, 5);
}
