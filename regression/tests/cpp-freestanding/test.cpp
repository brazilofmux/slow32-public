// Freestanding C++ through the SLOW-32 backend: no RTTI, no C++ standard
// library; new/delete, guards and pure virtuals from runtime/cxxabi.c.  Self-checking; prints PASS/FAIL per part and
// exits with the number of failures.
typedef decltype(sizeof 0) size_t;
extern "C" {
int printf(const char *, ...);
void *malloc(size_t);
void free(void *);
}

// new/delete, __cxa_pure_virtual and the static guards come from the
// runtime's cxxabi.c; placement new is the header's job, done here.
inline void *operator new(size_t, void *p) noexcept { return p; }

static int fails;
static void check(const char *what, bool ok) {
    printf("%-28s %s\n", what, ok ? "PASS" : "FAIL");
    if (!ok) fails++;
}

// --- a global with a constructor: runs before main through .init_array
static int ctor_order[4];
static int ctor_n;
struct Early {
    int v;
    Early(int x) : v(x * 3) { ctor_order[ctor_n++] = x; }
    ~Early() {}
};
static Early early_a(7);
static Early early_b(11);

// --- virtual dispatch, pure virtuals, virtual destructors
static int dtor_log;
struct Shape {
    virtual ~Shape() { dtor_log += 1; }
    virtual int area() const = 0;
    virtual int sides() const { return 0; }
};
struct Rect : Shape {
    int w, h;
    Rect(int w, int h) : w(w), h(h) {}
    ~Rect() override { dtor_log += 10; }
    int area() const override { return w * h; }
    int sides() const override { return 4; }
};
struct Square : Rect {
    Square(int s) : Rect(s, s) {}
    ~Square() override { dtor_log += 100; }
};
struct Tri : Shape {
    int b, h;
    Tri(int b, int h) : b(b), h(h) {}
    int area() const override { return b * h / 2; }
    int sides() const override { return 3; }
};

// --- multiple inheritance: this-pointer adjustment through thunks
struct Named { virtual const char *name() const { return "named"; } int tag = 5; };
struct Both : Named, Rect {
    Both() : Rect(2, 9) {}
    const char *name() const override { return "both"; }
    int area() const override { return Rect::area() + tag; }
};

// --- templates: a small vector, a generic algorithm, a specialization
template <typename T> struct Vec {
    T *d = nullptr; int n = 0, cap = 0;
    ~Vec() { for (int i = 0; i < n; i++) d[i].~T(); free(d); }
    void push(const T &v) {
        if (n == cap) {
            int nc = cap ? cap * 2 : 4;
            T *nd = (T *)malloc(sizeof(T) * nc);
            for (int i = 0; i < n; i++) { new (&nd[i]) T(d[i]); d[i].~T(); }
            free(d); d = nd; cap = nc;
        }
        new (&d[n++]) T(v);
    }
    T &operator[](int i) { return d[i]; }
    T *begin() { return d; }
    T *end() { return d + n; }
};
template <typename It, typename F> void sort_by(It b, It e, F less) {
    for (It i = b; i != e; ++i)
        for (It j = i + 1; j != e; ++j)
            if (less(*j, *i)) { auto t = *i; *i = *j; *j = t; }
}
template <typename T> T sum(Vec<T> &v) { T s{}; for (auto &x : v) s += x; return s; }
template <int N> struct Fact { static const long long v = N * Fact<N - 1>::v; };
template <> struct Fact<0> { static const long long v = 1; };

// --- operator overloading, references, a value type with copies
struct Frac {
    long long num, den;
    Frac(long long n = 0, long long d = 1) : num(n), den(d) { norm(); }
    void norm() {
        long long a = num < 0 ? -num : num, b = den;
        while (b) { long long t = a % b; a = b; b = t; }
        if (a) { num /= a; den /= a; }
    }
    Frac operator+(const Frac &o) const { return Frac(num * o.den + o.num * den, den * o.den); }
    Frac &operator+=(const Frac &o) { *this = *this + o; return *this; }
    bool operator==(const Frac &o) const { return num == o.num && den == o.den; }
};

// --- RAII: destructors at scope exit, in reverse order
static char trail[16]; static int trail_n;
struct Guard { char c; Guard(char c) : c(c) {} ~Guard() { trail[trail_n++] = c; } };

// --- function-local static: initialised once, through __cxa_guard_*
static int counter_inits;
static int next_id() {
    static int base = (counter_inits++, 1000);
    return base++;
}

static double lerp(double a, double b, double t) { return a + (b - a) * t; }

int main() {
    check("global ctors before main", ctor_n == 2 && ctor_order[0] == 7 && ctor_order[1] == 11 &&
                                      early_a.v == 21 && early_b.v == 33);

    Shape *s[3] = { new Rect(3, 4), new Square(5), new Tri(6, 4) };
    int areas = 0, sides = 0;
    for (Shape *p : s) { areas += p->area(); sides += p->sides(); }
    check("virtual dispatch", areas == 12 + 25 + 12 && sides == 4 + 4 + 3);
    dtor_log = 0; delete s[1];
    check("virtual destructor chain", dtor_log == 111);
    delete s[0]; delete s[2];

    Both both;
    Named *nm = &both; Shape *sh = &both;
    check("multiple inheritance", nm->name()[0] == 'b' && sh->area() == 23 && sh->sides() == 4);

    Vec<int> v;
    for (int i = 0; i < 20; i++) v.push((i * 7919) % 101);
    sort_by(v.begin(), v.end(), [](int a, int b) { return a < b; });
    bool sorted = true;
    for (int i = 1; i < v.n; i++) if (v[i - 1] > v[i]) sorted = false;
    check("template vector + lambda sort", sorted && v.n == 20 && sum(v) == 922);

    int bias = 3;
    auto add = [&bias](int x) { return x + bias; };
    bias = 10;
    check("lambda capture by reference", add(5) == 15);

    check("template recursion (constexpr)", Fact<15>::v == 1307674368000LL);

    Vec<Frac> fr;
    for (int k = 1; k <= 6; k++) fr.push(Frac(1, k * (k + 1)));
    Frac t; for (auto &f : fr) t += f;
    check("value type, 64-bit math", t == Frac(6, 7));

    { Guard a('a'); { Guard b('b'); Guard c('c'); } Guard d('d'); }
    trail[trail_n] = 0;
    check("RAII reverse destruction", trail[0] == 'c' && trail[1] == 'b' && trail[2] == 'd' && trail[3] == 'a');

    double *arr = new double[5];
    for (int i = 0; i < 5; i++) arr[i] = lerp(1.0, 3.0, i / 4.0);
    check("new[] + double math", arr[2] == 2.0 && arr[4] == 3.0);
    delete[] arr;

    int a1 = next_id(), a2 = next_id(), a3 = next_id();
    check("function-local static", counter_inits == 1 && a1 == 1000 && a3 == 1002 && a2 == 1001);

    printf("%d failed\n", fails);
    return fails;
}
