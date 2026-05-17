#define ADD(a, b) ((a) + (b))
#define MUL(a, b) ((a) * (b))
#define ADD3(x) ADD(MUL((x), 2), 1)

int main(void) {
    int v = ADD3(4);
    return (v == 9) ? 0 : 1;
}
