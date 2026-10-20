// operator new at its edges, from the runtime's cxa_support.c and
// cxa_std.cpp: new T[0] owes a pointer that is not null and is not any
// other live pointer, and new (std::nothrow) says null when the heap
// cannot serve it -- for a request too big for the address space, and for
// an array count whose byte size overflows, which clang passes on as
// SIZE_MAX.  (The plain form traps instead; a trap ends the run, so it is
// not tried here.)  Sizes and pointers go through volatiles so the
// optimizer cannot fold an allocation away.  Self-checking; prints
// PASS/FAIL per part and exits with the number of failures.
#include <new>
extern "C" int printf(const char *, ...);

static int fails;
static void check(const char *what, bool ok) {
    printf("%-34s %s\n", what, ok ? "PASS" : "FAIL");
    if (!ok) fails++;
}

static volatile unsigned big_bytes = 0x10000000u;
static volatile unsigned big_count = 0x40000001u;
static volatile unsigned none = 0;
static void *volatile keep_a, *volatile keep_b;

struct Gauge {
    int reading;
    char tag[12];
};

int main() {
    char *a = new char[none];
    char *b = new char[none];
    keep_a = a;
    keep_b = b;
    check("new char[0] not null", keep_a && keep_b);
    check("two new char[0] differ", keep_a != keep_b);
    delete[] a;
    delete[] b;

    char *huge = new (std::nothrow) char[big_bytes];
    keep_a = huge;
    check("nothrow 256 MB array is null", keep_a == nullptr);

    int *wrap = new (std::nothrow) int[big_count];
    keep_a = wrap;
    check("nothrow overflowing count is null", keep_a == nullptr);

    char *zero = new (std::nothrow) char[none];
    keep_a = zero;
    check("nothrow char[0] not null", keep_a != nullptr);
    delete[] zero;

    Gauge *g = new (std::nothrow) Gauge{41, "quartzite"};
    keep_a = g;
    check("nothrow struct served", keep_a != nullptr && g->reading == 41 &&
                                       g->tag[0] == 'q' && g->tag[8] == 'e');
    delete g;

    int *row = new (std::nothrow) int[6];
    keep_a = row;
    bool ok = keep_a != nullptr;
    if (ok) {
        for (int i = 0; i < 6; i++) row[i] = i * i + 3;
        ok = row[0] == 3 && row[5] == 28;
    }
    check("nothrow int[6] served and usable", ok);
    delete[] row;

    printf("%d failed\n", fails);
    return fails;
}
