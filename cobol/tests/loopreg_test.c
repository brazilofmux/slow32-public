/* host test for src/cobc/loopreg.h: the reading of a loop's code that
 * decides whether an item may be kept in a register across it.
 *
 * The decision is "nothing here can store into the item but its own
 * marked stores", and most of what it must refuse the compiler does not
 * emit today -- a register that holds one address by one path and another
 * by the other, a store through a register left over from before a call,
 * a mark that is wrong.  A rule nothing exercises is a rule that can be
 * broken without a test noticing (the first mutation run: twenty of
 * forty mutants of the analysis survived every COBOL program).  So the
 * analysis is given lines directly: each case is a region's code, the
 * items asked about, and for each whether something else may store into
 * it and how many of its marked loads and stores are what they say.
 *
 * The compiler is one translation unit; this includes it. */
#define main s32_cobc_main
#include "../src/s32-cobc.c"
#undef main

typedef struct { const char *label; long off; int size; int conflict, nl, ns; } Want;
typedef struct { const char *name; const char *code; Want w[3]; } Case;

/* X is ws0_1 (two bytes at 0); Y is ws0_2; Z is ws0_1+4, in X's record */
#define X   { "ws0_1", 0, 2, 0, 0, 0 }
#define XC  { "ws0_1", 0, 2, 1, 0, 0 }
#define LAX "\tlui r3, %hi(ws0_1)\n\taddi r3, r3, %lo(ws0_1)\n"
#define LAY "\tlui r3, %hi(ws0_2)\n\taddi r3, r3, %lo(ws0_2)\n"
#define LDX "#@L 0 0 r1 r3\n\tldbu r2, r3+1\n\tldbu r1, r3+0\n\tslli r1, r1, 8\n\tor r1, r1, r2\n#@.\n"
#define STX "#@S 0 0 r1 r3\n\tstb r3+1, r1\n\tsrli r2, r1, 8\n\tstb r3+0, r2\n#@.\n"

static const Case cases[] = {
 { "a load, an add, a store: the item's own",
   LAX LDX "\taddi r1, r1, 1\n" STX, { { "ws0_1", 0, 2, 0, 1, 1 } } },
 { "the truncation's branch between the load and the store: the address is the same by both paths",
   LAX LDX "\taddi r1, r1, 1\n\tlui r2, 2\n\taddi r2, r2, 1808\n\tbltu r1, r2, .L5\n\tsub r1, r1, r2\n.L5:\n" STX, { { "ws0_1", 0, 2, 0, 1, 1 } } },
 { "a store into another record", LAY "\tstb r3+0, r1\n", { X } },
 { "a store at the item, unmarked", LAX "\tstb r3+0, r1\n", { XC } },
 { "... at its second byte", LAX "\tstb r3+1, r1\n", { XC } },
 { "... a word that begins before it and covers it", "\tlui r3, %hi(ws0_1+2)\n\taddi r3, r3, %lo(ws0_1+2)\n\tstw r3+0, r1\n", { { "ws0_1", 4, 2, 1, 0, 0 }, { "ws0_1", 0, 2, 0, 0, 0 }, { "ws0_1", 6, 2, 0, 0, 0 } } },
 { "... a byte beside it in the same record", "\tlui r3, %hi(ws0_1+2)\n\taddi r3, r3, %lo(ws0_1+2)\n\tstb r3+0, r1\n", { X } },
 { "an address moved on by a constant", LAX "\taddi r3, r3, 4\n\tstb r3+0, r1\n", { X, { "ws0_1", 4, 2, 1, 0, 0 } } },
 { "an element of its record: any of it", LAX "\tadd r3, r3, r11\n\tstb r3+0, r1\n", { XC, { "ws0_1", 40, 2, 1, 0, 0 }, { "ws0_2", 0, 2, 0, 0, 0 } } },
 { "... index first", LAX "\tadd r3, r11, r3\n\tstb r3+0, r1\n", { XC } },
 { "an address less an index", LAX "\tsub r3, r3, r11\n\tstb r3+0, r1\n", { XC } },
 { "a store through an address out of storage", LAY "\tldw r3, r3+0\n\tstb r3+0, r1\n", { XC } },
 { "a store through a register nobody set", "\tstb r7+0, r1\n", { XC } },
 { "an address copied", LAX "\tadd r5, r3, r0\n\tstb r5+0, r1\n", { XC } },
 { "... the other way", LAX "\tadd r5, r0, r3\n\tstb r5+0, r1\n", { XC } },
 { "an address through the frame", LAY "\tstw sp+12, r3\n" LAX "\tldw r4, sp+12\n\tstb r4+0, r1\n", { X } },
 { "... the item's own", LAX "\tstw sp+12, r3\n" LAY "\tldw r4, sp+12\n\tstb r4+0, r1\n", { XC } },
 { "... a byte stored in the frame: its words are not known after", LAY "\tstw sp+12, r3\n\tstb sp+13, r1\n\tldw r4, sp+12\n\tstb r4+0, r1\n", { XC } },
 { "a routine that stores nothing", LAX "\tjal r31, cob_display_field\n", { X } },
 { "a routine nobody lists", "\tjal r31, cob_accept\n", { XC } },
 { "memcpy to another record", LAY "\tjal r31, memcpy\n", { X } },
 { "memcpy to the item", LAX "\tjal r31, memcpy\n", { XC } },
 { "memcpy to below the item, how far not known", LAX "\taddi r3, r3, -2\n\tjal r31, memcpy\n", { XC } },
 { "memcpy to beyond the item", LAX "\taddi r3, r3, 2\n\tjal r31, memcpy\n", { X } },
 { "memcpy to an element of its record", LAX "\tadd r3, r3, r11\n\tjal r31, memcpy\n", { XC } },
 { "memcpy to who knows where", "\tldw r3, sp+20\n\tjal r31, memcpy\n", { XC } },
 { "cob_move: its third argument", "\tlui r5, %hi(ws0_1)\n\taddi r5, r5, %lo(ws0_1)\n" LAY "\tjal r31, cob_move\n", { XC } },
 { "... the first is read", LAX "\tlui r5, %hi(ws0_2)\n\taddi r5, r5, %lo(ws0_2)\n\tjal r31, cob_move\n", { X } },
 { "a register is nobody's after a call", LAY "\tjal r31, cob_display_nl\n\tstb r3+0, r1\n", { XC } },
 { "... r11 is still the caller's", "\tlui r11, %hi(ws0_2)\n\taddi r11, r11, %lo(ws0_2)\n\tjal r31, cob_display_nl\n\tstb r11+0, r1\n", { X } },
 { "one address by one path, another by the other", LAY "\tbeq r1, r0, .L7\n" LAX ".L7:\n\tstb r3+0, r1\n", { XC } },
 { "... the item's by the jump, another's by the fall", LAX "\tbeq r1, r0, .L7\n" LAY ".L7:\n\tstb r3+0, r1\n", { XC } },
 { "... an address by one path, a number by the other", LAY "\tbeq r1, r0, .L7\n\taddi r3, r0, 5\n.L7:\n\tstb r3+0, r1\n", { XC } },
 { "... two places in one record: somewhere in it", LAX "\tbeq r1, r0, .L7\n\taddi r3, r3, 4\n.L7:\n\tstb r3+0, r1\n", { XC, { "ws0_1", 40, 2, 1, 0, 0 }, { "ws0_2", 0, 2, 0, 0, 0 } } },
 { "... the same by both", LAY "\tbeq r1, r0, .L7\n" LAY ".L7:\n\tstb r3+0, r1\n", { X } },
 { "... by a jump and a fall", LAY "\tbeq r1, r0, .L7\n\taddi r1, r0, 1\n\tjal r0, .L8\n.L7:\n" LAX ".L8:\n\tstb r3+0, r1\n", { XC } },
 { "a label only jumped to: what the jump brought", LAY "\tjal r0, .L9\n" LAX ".L9:\n\tstb r3+0, r1\n", { X } },
 { "the top of a loop: what the first pass brought is not what the second does", LAY ".L3:\n\tstb r3+0, r1\n" LAX "\tbne r1, r0, .L3\n", { XC } },
 { "a label jumped to from above and from below: the one from below is not known yet", LAY "\tjal r0, .L3\n.L3:\n\tstb r3+0, r1\n" LAX "\tbne r1, r0, .L3\n", { XC } },
 { "a label whose address is taken", LAY "\tlui r4, %hi(.L4)\n\taddi r4, r4, %lo(.L4)\n\tbeq r1, r0, .L4\n.L4:\n\tstb r3+0, r1\n", { XC } },
 { "a label nothing here jumps to", LAY ".L6:\n\tstb r3+0, r1\n", { XC } },
 { "a profile's label is a name, not a place to come to", LAY "__ln_12_3:\n\tstb r3+0, r1\n", { X } },
 { "a jump to a paragraph", "\tjal r0, .Lp0_3\n", { XC } },
 { "a branch to a paragraph", "\tbeq r1, r0, .Lp0_3\n", { XC } },
 { "a jump through a register", "\tjalr r0, r1, 0\n", { XC } },
 { "a call through a register", "\tjalr r31, r1, 0\n", { XC } },
 { "the frame moved", "\taddi sp, sp, -16\n", { XC } },
 { "an instruction not listed", "\tfrob r1, r2\n", { XC } },
 { "a load's mark on another item's address: not a load of the item", LAY LDX, { { "ws0_1", 0, 2, 0, 0, 0 } } },
 /* a store's mark that does not hold is left as it is -- a store the register would not follow: the item is refused */
 { "a store's mark on another item's address: a store there", LAY STX, { { "ws0_1", 0, 2, 1, 0, 0 }, { "ws0_2", 0, 2, 1, 0, 0 } } },
 { "a store's mark, the store one byte past", LAX "#@S 0 0 r1 r3\n\tstb r3+2, r1\n#@.\n", { { "ws0_1", 0, 2, 1, 0, 0 }, { "ws0_1", 2, 2, 1, 0, 0 } } },
 { "a store's mark, a good store and then a label", LAX "#@S 0 0 r1 r3\n\tstb r3+1, r1\n.L2:\n\tstb r3+0, r1\n#@.\n", { XC } },
 { "a store's mark, the address made inside it", "#@S 0 0 r1 r3\n" LAX "\tstb r3+0, r1\n#@.\n", { XC } },
 { "a marked store counts against what shares its bytes", LAX STX, { { "ws0_1", 0, 2, 0, 0, 1 }, { "ws0_1", 1, 1, 1, 0, 0 } } },
 { "a load's mark, a load from elsewhere in it", LAX "#@L 0 0 r1 r3\n\tldbu r1, r4+0\n#@.\n", { { "ws0_1", 0, 2, 0, 0, 0 } } },
 { "a load's mark with a label in it", LAX "#@L 0 0 r1 r3\n\tldbu r1, r3+0\n.L2:\n#@.\n", { { "ws0_1", 0, 2, 0, 0, 0 } } },
};

/* lr_after: what becomes of a register in the lines that follow */
static const struct { const char *name; const char *code; int reg, want; } after[] = {
 { "written first", "\tlui r3, %hi(ws0_2)\n", 3, 1 },
 { "read first", "\taddi r4, r3, 1\n\tlui r3, 5\n", 3, 0 },
 { "read and written by one instruction", "\taddi r3, r3, 1\n", 3, 0 },
 { "read as a store's base", "\tstb r3+0, r1\n", 3, 0 },
 { "read as a store's value", "\tstb r4+0, r3\n", 3, 0 },
 { "read as a load's base", "\tldbu r1, r3+0\n", 3, 0 },
 { "written by a load", "\tldbu r3, r4+0\n", 3, 1 },
 { "a branch that reads it", "\tbeq r3, r0, .L1\n", 3, 0 },
 { "a branch first", "\tbeq r1, r0, .L1\n\tlui r3, 5\n", 3, 2 },
 { "a call first", "\tjal r31, cob_display_nl\n", 3, 2 },
 { "a label first", ".L1:\n\tlui r3, 5\n", 3, 2 },
 { "nothing more", "\taddi r1, r1, 1\n", 3, 2 },
 { "a mark is not a line", "#@.\n\tlui r3, 5\n", 3, 1 },
};

static int load(const char *code)
{
    g_nasm = 0;
    const char *p = code;
    while (*p) {
        const char *e = strchr(p, '\n');
        emit("%.*s", (int)(e - p), p);
        p = e + 1;
    }
    return g_nasm;
}

int main(void)
{
    int bad = 0, n = 0;
    for (size_t c = 0; c < sizeof cases / sizeof *cases; c++) {
        LrItem it[3]; int nit = 0;
        memset(it, 0, sizeof it);
        for (int k = 0; k < 3 && cases[c].w[k].label; k++) {
            it[k].sym = k; it[k].off = cases[c].w[k].off; it[k].size = cases[c].w[k].size; it[k].reg = -1;
            snprintf(it[k].label, sizeof it[k].label, "%s", cases[c].w[k].label);
            nit++;
        }
        /* the marks in the cases name item 0 at offset 0: lr_find goes by (sym, off) */
        it[0].sym = 0;
        int nl = load(cases[c].code);
        unsigned char *ok = calloc((size_t)nl + 1, 1);
        lr_scan(0, nl, it, nit, ok);
        for (int k = 0; k < nit; k++) {
            const Want *w = &cases[c].w[k];
            n++;
            if (it[k].conflict != w->conflict || it[k].nl != w->nl || it[k].ns != w->ns) {
                printf("FAIL %s: item %d (%s+%ld): conflict %d loads %d stores %d, want %d %d %d\n", cases[c].name, k, w->label, w->off,
                       it[k].conflict, it[k].nl, it[k].ns, w->conflict, w->nl, w->ns);
                bad++;
            }
        }
        free(ok);
    }
    for (size_t c = 0; c < sizeof after / sizeof *after; c++) {
        int nl = load(after[c].code), got = lr_after(0, nl, after[c].reg);
        n++;
        if (got != after[c].want) { printf("FAIL lr_after, %s: %d, want %d\n", after[c].name, got, after[c].want); bad++; }
    }
    /* a WRITE and a READ: the file's status item, its relative key, its
     * record area, its block; an EXTERNAL file's, which may be another
     * program's: anything */
    {
        static Sym sy[4]; static File fl[2];
        g_sym = sy; g_nsym = 4; g_files = fl; g_nfile = 2;
        for (int k = 0; k < 4; k++) { sy[k].record = k; sy[k].size = 2; sy[k].offset = 0; snprintf(sy[k].label, sizeof sy[k].label, "ws0_%d", k + 1); }
        fl[0].unit = 0; fl[0].status_sym = &sy[1]; fl[0].relkey_sym = &sy[2]; fl[0].rec = 3; fl[0].dep_sym = NULL;
        fl[1] = fl[0]; fl[1].external = 1;
        static const struct { const char *name, *code; int c[5]; } fc[] = {
            /* items: ws0_1 (nobody's), ws0_2 (the status), ws0_3 (the relative key), ws0_4 (the record area), .Lf0_0+136 (LINAGE-COUNTER) */
            { "WRITE", "\tlui r3, %hi(.Lf0_0)\n\taddi r3, r3, %lo(.Lf0_0)\n\tjal r31, cob_write\n", { 0, 1, 1, 0, 1 } },
            { "READ", "\tlui r3, %hi(.Lf0_0)\n\taddi r3, r3, %lo(.Lf0_0)\n\tjal r31, cob_read\n", { 0, 1, 1, 1, 1 } },
            { "WRITE, the file EXTERNAL", "\tlui r3, %hi(.Lf0_1)\n\taddi r3, r3, %lo(.Lf0_1)\n\tjal r31, cob_write\n", { 1, 1, 1, 1, 1 } },
            { "WRITE, the file not named", "\tldw r3, sp+12\n\tjal r31, cob_write\n", { 1, 1, 1, 1, 1 } },
            { "WRITE, a file of another unit's", "\tlui r3, %hi(.Lf7_0)\n\taddi r3, r3, %lo(.Lf7_0)\n\tjal r31, cob_write\n", { 1, 1, 1, 1, 1 } },
        };
        for (size_t c = 0; c < sizeof fc / sizeof *fc; c++) {
            LrItem it[5]; memset(it, 0, sizeof it);
            for (int k = 0; k < 4; k++) { it[k].sym = k; it[k].size = 2; it[k].reg = -1; snprintf(it[k].label, sizeof it[k].label, "ws0_%d", k + 1); }
            it[4].sym = 9; it[4].off = 136; it[4].size = 4; it[4].reg = -1; snprintf(it[4].label, sizeof it[4].label, ".Lf0_0");
            int nl = load(fc[c].code);
            unsigned char *ok = calloc((size_t)nl + 1, 1);
            lr_scan(0, nl, it, 5, ok);
            for (int k = 0; k < 5; k++) {
                n++;
                if (it[k].conflict != fc[c].c[k]) { printf("FAIL %s: item %d: conflict %d, want %d\n", fc[c].name, k, it[k].conflict, fc[c].c[k]); bad++; }
            }
            free(ok);
        }
    }
    printf("loopreg_test: %d checks, %d failed\n", n, bad);
    return bad != 0;
}
