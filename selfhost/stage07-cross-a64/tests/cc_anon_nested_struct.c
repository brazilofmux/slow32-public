struct outer {
    int head[16];
    struct {
        int a;
        int b;
    } inner;
    int used;
    int later[16];
    int tail;
};

int main(void) {
    struct outer o = {0};
    o.later[3] = 7;
    o.tail = 1;
    return o.later[3] == 7 && o.tail == 1;
}
