// C++ exceptions through the compiler of the day: unwinding through
// cleanup frames (_Unwind_Resume, callee-saved registers restored from the
// CFI), a second handler in one try, rethrow, catch (...), nested try.
// Self-checking; expected.txt is what a host build of this file prints.
extern "C" int printf(const char *, ...);

static int fails;
static void check(const char *what, bool ok) {
    printf("%-34s %s\n", what, ok ? "PASS" : "FAIL");
    if (!ok) fails++;
}

static int dtors;
struct Noisy { int id; Noisy(int i) : id(i) {} ~Noisy() { dtors += id; } };

struct Oops { int code; };
struct Base { int b = 1; virtual ~Base() {} };
struct Derived : Base { int d = 2; };

static void deep(int n) { Noisy x(n); if (n == 0) throw Oops{42}; deep(n - 1); }
static int thrower(int v) { if (v > 2) throw v; return v; }
static void rethrow_it() { try { throw Oops{7}; } catch (Oops &) { throw; } }

int main() {
    int got = 0;
    try { thrower(5); } catch (int v) { got = v; }
    check("throw int, catch int", got == 5);

    got = 0;
    try { thrower(1); got = 1; } catch (int) { got = -1; }
    check("no throw, no catch", got == 1);

    dtors = 0; got = 0;
    try { deep(4); } catch (Oops &o) { got = o.code; }
    check("unwind 5 frames, run destructors", got == 42 && dtors == 4 + 3 + 2 + 1 + 0);

    got = 0;
    try { rethrow_it(); } catch (Oops &o) { got = o.code; }
    check("rethrow with throw;", got == 7);

    got = 0;
    try { throw 3.5; } catch (int) { got = 1; } catch (double d) { got = d == 3.5 ? 2 : 3; }
    check("second handler by type", got == 2);

    got = 0;
    try { throw "words"; } catch (...) { got = 9; }
    check("catch (...)", got == 9);

    got = 0;
    try { try { throw 11; } catch (double) { got = -1; } } catch (int v) { got = v; }
    check("outer try catches what inner skips", got == 11);

    got = 0;
    int (*fp)(int) = thrower;
    try { fp(8); } catch (int v) { got = v; }
    check("throw through a function pointer", got == 8);

    got = 0;
    try { throw Derived(); } catch (Derived &d) { got = d.d; }
    check("catch class by its own type", got == 2);


    printf("%d failed\n", fails);
    return fails;
}
