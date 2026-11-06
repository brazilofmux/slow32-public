/* The C half of abi.f90: clang's layout is what Fortran has to match. */
#include <stddef.h>

struct mixed {
    signed char a;
    double b;
    int c;
    long long d;
    void *e;
    int (*f)(void *, int);
    long g;
    size_t h;
};

size_t c_sizes(int which)
{
    switch (which) {
    case 0: return sizeof(struct mixed);
    case 1: return sizeof(long);
    case 2: return sizeof(size_t);
    default: return sizeof(void *);
    }
}

int c_check(const struct mixed *m, size_t n, long k, int last)
{
    if (m->a != 11) return 1;
    if (m->b != 1.5) return 2;
    if (m->c != 33) return 3;
    if (m->d != 4400000000LL) return 4;
    if (*(int *)m->e != 40) return 5;
    if (m->f(m->e, 1) != 41) return 6;
    if (m->g != 77) return 7;
    if (m->h != 88) return 8;
    if (n != 1234) return 9;
    if (k != 5678) return 10;
    if (last != 91) return 11;
    return 0;
}

int c_apply(int (*f)(void *, int), void *p, int x)
{
    return f(p, x);
}
