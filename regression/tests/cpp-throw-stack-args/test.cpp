// An exception thrown through a function that passes arguments on the
// stack and has no frame pointer.  Around such a call SP moves below the
// frame the prologue built; the CFI must follow it (.cfi_adjust_cfa_offset,
// which the backend did not emit and slow32asm did not read until
// 2026-10-20), or the unwinder takes the outgoing arguments for the saved
// LR, loses the frame, and the exception reaches no handler: libc++'s
// __throw_system_error, which passes an error_code on the stack, threw
// exceptions nothing could catch.  The relay below is that shape: a frame
// of its own, ten arguments to the thrower, two of them on the stack.
// Self-checking; expected.txt is what a host build of this file prints.
extern "C" int printf(const char *, ...);

static int fails;
static void check(const char *what, bool ok) {
    printf("%-34s %s\n", what, ok ? "PASS" : "FAIL");
    if (!ok) fails++;
}

struct Fault { int sum; };

__attribute__((noinline)) static void hurl(int a, int b, int c, int d, int e,
                                           int f, int g, int h, int i, int j) {
    throw Fault{a + b + c + d + e + f + g + h + i + j};
}

__attribute__((noinline)) static int relay(int base) {
    hurl(base, base + 1, base + 2, base + 3, base + 4, base + 5, base + 6,
         base + 7, base + 8, base + 9);
    return -1;
}

__attribute__((noinline)) static int outer(int base) {
    int r = 0;
    for (int k = 0; k < 3; k++) {
        try {
            r += relay(base + k);
        } catch (const Fault &f) {
            r += f.sum;
        }
    }
    return r;
}

int main() {
    int got = -2;
    try {
        got = relay(10);
    } catch (const Fault &f) {
        got = f.sum;
    }
    check("caught through stack-arg relay", got == 145);
    check("three catches in a loop", outer(1) == 55 + 65 + 75);
    printf("%d failed\n", fails);
    return fails;
}
