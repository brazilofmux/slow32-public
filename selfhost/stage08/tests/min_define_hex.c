#define MASK 0xFF
int main(void) {
    int x;
    x = 256 + MASK;
    return x & MASK;
}
