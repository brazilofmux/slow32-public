/* Function pointer table dispatched in a loop.  Indirect calls are
 * a different codegen shape from direct calls: the target is in a
 * register, so the regalloc has to pick a register that survives
 * the call (callee-saved) or save the loaded fnptr across uses. */
static int op_add(int a, int b) { return a + b; }
static int op_sub(int a, int b) { return a - b; }
static int op_mul(int a, int b) { return a * b; }
static int op_xor(int a, int b) { return a ^ b; }
static int op_and(int a, int b) { return a & b; }
static int op_or (int a, int b) { return a | b; }

typedef int (*op_fn)(int, int);

int main(void) {
    op_fn ops[6];
    ops[0] = op_add;
    ops[1] = op_sub;
    ops[2] = op_mul;
    ops[3] = op_xor;
    ops[4] = op_and;
    ops[5] = op_or;

    int acc = 0;
    int i;
    for (i = 0; i < 200; i++) {
        op_fn f = ops[i % 6];
        int a = (i * 7) ^ 0x55;
        int b = (i * 11) ^ 0xA5;
        acc = acc * 13 + f(a, b);
        acc = acc - f(b, a);
    }
    return acc & 0xff;
}
