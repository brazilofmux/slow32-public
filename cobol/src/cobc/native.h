/* s32-cobc: items written the machine's way.  A part of one translation
 * unit, included by s32-cobc.c in order; not a header to include
 * anywhere else. */

/* ====================================================================== */
/* Native items (docs/plans/census.md, step 2)                             */
/* ====================================================================== */

/* An item that stands alone, and whose every use is a use of its number
 * (census.h: cen_native_cand), has no layout the program can observe.
 * It is given the machine's: a DISPLAY or COMP-3 number becomes a binary
 * one, of the size a COMP item of its picture has, in the first bytes of
 * the place it had (cen_native_size); a COMP item keeps its bytes and
 * loses its byte order.  Its picture still says how many digits it holds
 * and where its point is -- a store truncates and a size error is raised
 * as before -- and everything that takes an item by its descriptor is
 * told the truth about it.  The record keeps its length and every other
 * item its place.  An item whose own bytes are too few for its binary
 * form -- a packed one of five digits, or of ten to thirteen -- is given
 * a cell in a record of the compiler's making, and its old place is a
 * hole nothing looks at.
 *
 * The verdict needs the whole PROCEDURE DIVISION, and the code of its
 * first statement needs the verdict.  The emitter writes code as it
 * reads, so the program is read twice: once in a child of this process
 * (fork), which compiles it as written with the census on and sends back
 * the items that may change, and once here, with those items changed
 * before any statement is compiled.  The child is the whole compiler
 * with nothing to undo; when a unit is a tree before it is code
 * (lowering to HIR) the tree will be asked instead, and this goes.
 *
 * -fno-native-items: every item as written (and no child). */

static int g_native_on = 1;
static int g_native_child;              /* this process is the one taking the census */
static int g_native_fd = -1;            /* ... and where it sends the verdicts */
typedef struct { int unit, sym; unsigned h; } NatRec;
static NatRec *g_nat; static int g_nnat;

static unsigned nat_hash(const Sym *s)
{
    unsigned h = 2166136261u;
    for (const char *p = s->name; *p; p++) h = (h ^ (unsigned char)*p) * 16777619u;
    return (h ^ (unsigned)s->line) * 16777619u ^ (unsigned)s->offset;
}

/* the child, at a unit's end: the items that may be written the machine's way */
static void native_verdicts(const char *native, int stride)
{
    if (!g_native_child || !native || g_nerrors) return;     /* a program with errors is compiled as written: its messages are about what was written */
    for (int i = g_sym_base; i < g_nsym; i++) {
        if (!native[(size_t)(i - g_sym_base) * (size_t)stride]) continue;
        NatRec r; r.unit = g_unit; r.sym = i; r.h = nat_hash(&g_sym[i]);
        const char *p = (const char *)&r; size_t left = sizeof r;
        while (left) { ssize_t w = write(g_native_fd, p, left); if (w <= 0) _exit(1); p += w; left -= (size_t)w; }
    }
}

/* before anything is compiled: the census, by a child */
static void native_prepass(void)
{
    if (!g_native_on || g_fnsig_only) return;
    int fd[2];
    if (pipe(fd)) return;
    fflush(NULL);
    pid_t pid = fork();
    if (pid < 0) { close(fd[0]); close(fd[1]); return; }      /* no child: every item as written */
    if (pid == 0) {
        close(fd[0]);
        g_native_child = 1; g_native_fd = fd[1]; g_cen_on = 1;
        if (!freopen("/dev/null", "w", stderr)) _exit(1);       /* what it has to say, the parent will say */
        return;
    }
    close(fd[1]);
    g_cen_on = 0;                       /* the census is the child's: of the program as it is written */
    size_t cap = 0, len = 0; char *buf = NULL;
    for (;;) {
        if (len + 4096 > cap) { cap = cap ? 2 * cap : 16384; buf = realloc(buf, cap); }
        ssize_t n = read(fd[0], buf + len, cap - len);
        if (n <= 0) break;
        len += (size_t)n;
    }
    close(fd[0]);
    int st; waitpid(pid, &st, 0);
    g_nnat = (int)(len / sizeof *g_nat);
    g_nat = (NatRec *)buf;
}

/* The cells of a unit's items that leave their records: one record the
 * compiler makes, each item at the alignment of its size.  It is a
 * record like any other from here on -- emitted with the unit's data,
 * with its initial image, put back by CANCEL and by an INITIAL program's
 * entry as the others are. */
static Sym *g_nat_cells; static int g_nat_cells_unit = -1;
static int native_cell(int size)
{
    if (g_nat_cells_unit != g_unit) {
        Sym *r = sym_new();
        snprintf(r->name, sizeof r->name, "native-items");
        r->level = 1; r->is_filler = 1; r->is_group = 1; r->record = sym_idx(r);
        snprintf(r->label, sizeof r->label, "wn%d", g_unit);
        g_nat_cells = r; g_nat_cells_unit = g_unit;
    }
    Sym *r = g_nat_cells;
    int off = (r->size + size - 1) / size * size;
    r->image = realloc(r->image, (size_t)off + (size_t)size);
    if (!r->image) { fprintf(stderr, "s32-cobc: out of memory\n"); exit(1); }
    memset(r->image + r->size, 0, (size_t)(off + size - r->size));
    r->size = r->image_size = off + size;
    return off;
}

static void native_flip(Sym *s)
{
    int inplace, size = cen_native_size(s, &inplace);
    if (!size) { fprintf(stderr, "s32-cobc: internal: '%s' cannot be written the machine's way\n", s->name); fail(); }
    s->usage = U_BINARY; s->has_usage = 1;
    s->size = size;
    s->native = 1; s->desc_id = -1;
    if (!inplace) {
        /* its own bytes are too few: a cell outside its record.  Nothing
         * names the groups over it, so nothing looks where it was. */
        int me = sym_idx(s), off = native_cell(size);
        s = &g_sym[me];
        s->record = sym_idx(g_nat_cells); s->offset = off; s->native = 2;     /* (its condition-names are reached through it) */
    }
    Sym *rec = &g_sym[s->record];
    if (rec->image) {
        /* its first value, written the new way: at every occurrence, for
         * an element of a table */
        int k[MAXDIM] = { 0 };
        for (;;) {
            int off = s->offset, d = s->ndims - 1;
            for (int q = 0; q < s->ndims; q++) off += k[q] * s->dim_stride[q];
            init_elem(s, rec->image + off, 1);
            while (d >= 0 && ++k[d] == s->dim_count[d]) k[d--] = 0;
            if (d < 0) break;
        }
    }
    if (getenv("S32_NATIVE_TRACE"))
        fprintf(stderr, "native: %s (line %d): %d byte%s%s%s\n", s->name, s->line, s->size, s->size == 1 ? "" : "s", inplace ? "" : ", out of its record",
                s->ndims ? ", each occurrence" : "");
}

/* a unit's DATA DIVISION is laid out, no statement is compiled yet */
static void native_apply(void)
{
    for (int k = 0; k < g_nnat; k++) {
        const NatRec *r = &g_nat[k];
        if (r->unit != g_unit) continue;
        if (r->sym < g_sym_base || r->sym >= g_nsym || nat_hash(&g_sym[r->sym]) != r->h) {
            fprintf(stderr, "s32-cobc: internal: the census names an item this compile does not have (unit %d, symbol %d)\n", r->unit, r->sym);
            fail();
        }
        native_flip(&g_sym[r->sym]);
    }
}
