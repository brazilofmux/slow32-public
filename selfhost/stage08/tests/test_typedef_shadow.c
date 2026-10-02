/* selfhost ISSUES-78: an object declared in a function hides a typedef
 * of the same name for the rest of its block (C99 6.2.1p4).
 *
 * The compiler asked only "is this identifier a typedef name", so after
 *     typedef struct { ... } host;
 *     ... { char host[128]; ... v ? v : (host) ... }
 * the parenthesized array was read as a cast and the expression after it
 * did not parse -- the COBOL runtime's SQL half, which this compiler
 * builds where there is no LLVM.  Where the "cast" did parse, it changed
 * the value: (port) - 1 was -1 cast to port, sizeof(host) the typedef's.
 *
 * Returns 0, or the number of the first check that fails. */
typedef struct { int a; int b; } host;
typedef int port;

static host g_tab[3];                   /* the typedef, at file scope */
static int takes(const char *host, int port) { return host[0] + (port) - 1; }
/* ... and after a function whose parameters had its name, where it is
 * the type again: an object, a member, a size, a cast, a parameter */
static host g_after;
static port g_port = 9;
struct after { host h; port p; };
static char sized[sizeof(host) + sizeof(port)];
static int cast_after = (port)3 + 4;
static int uses(host *h, port p) { return h->a + p; }

static int pick(int c, const char *v)
{
    char host[8];
    int port;
    host[0] = 'h'; host[1] = 0; port = 7;
    return (c ? v : (host))[0] + (port) - 1;
}

static int size_of_local(void) { char host[40]; host[0] = 0; return (int)sizeof(host) + host[0]; }

static int blocks(void)
{
    int r;
    { int host; host = 5; r = (host) - 1; }     /* hidden here ... */
    { host h; h.a = 3; h.b = 4; r = r * 10 + h.b + (int)sizeof(host); }   /* ... and back */
    return r;
}

static int after(void) { host h; port p; h.a = 1; h.b = 2; p = (port)3; return h.a + h.b + p; }

int main(void)
{
    if (pick(0, "v") != 'h' + 6) return 1;
    if (pick(1, "v") != 'v' + 6) return 2;
    if (takes("a", 5) != 'a' + 4) return 3;
    if (size_of_local() != 40) return 4;
    if (sizeof(host) != 8 || sizeof(g_tab) != 24) return 5;
    if (blocks() != 4 * 10 + 4 + 8) return 6;
    if (after() != 6) return 7;
    g_after.a = 2; g_tab[2].b = 3;
    if (g_after.a + g_tab[2].b + g_port != 14) return 8;
    if (sizeof(struct after) != 12 || sizeof(sized) != 12 || cast_after != 7) return 9;
    if (uses(&g_after, 5) != 7) return 10;
    return 0;
}
