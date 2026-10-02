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
        lr_scan(0, nl, it, nit, ok, NULL);
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
            lr_scan(0, nl, it, 5, ok, NULL);
            for (int k = 0; k < 5; k++) {
                n++;
                if (it[k].conflict != fc[c].c[k]) { printf("FAIL %s: item %d: conflict %d, want %d\n", fc[c].name, k, it[k].conflict, fc[c].c[k]); bad++; }
            }
            free(ok);
        }
    }
    /* The unit's reading: what the registers hold.  Items: sym 0 at ws0_1
     * (X), 1 at ws0_2 (Y), 2 to 5 at ws0_3 to ws0_6, all two-byte binary;
     * sym 6 at ws0_7, two DISPLAY digits.  For each mark in the code, in
     * order: U and the register a load takes its item from; D and the
     * register a load or store leaves it in, taken from there later; d
     * the same, never taken; - nothing. */
    {
        static Sym sy[8];
        memset(sy, 0, sizeof sy);
        g_sym = sy; g_nsym = 7; g_nfile = 0;
        for (int k = 0; k < 7; k++) {
            sy[k].record = k; sy[k].size = 2; sy[k].pi.category = PIC_NUMERIC; sy[k].pi.digits = k == 6 ? 2 : 4;
            sy[k].usage = k == 6 ? U_DISPLAY : U_BINARY;
            snprintf(sy[k].label, sizeof sy[k].label, "ws0_%d", k + 1);
        }
#define LA(n)   "\tlui r3, %hi(ws0_" #n ")\n\taddi r3, r3, %lo(ws0_" #n ")\n"
#define LD(s)   "#@L " #s " 0 r1 r3\n\tldbu r2, r3+1\n\tldbu r1, r3+0\n\tslli r1, r1, 8\n\tor r1, r1, r2\n#@.\n"
#define ST(s)   "#@S " #s " 0 r1 r3\n\tstb r3+1, r1\n\tsrli r2, r1, 8\n\tstb r3+0, r2\n#@.\n"
#define LOADX   LA(1) LD(0)
#define LOADY   LA(2) LD(1)
        static const struct { const char *name, *code, *want; } ac[] = {
            { "a load, and a load: the second from the register", LOADX LOADX, "D0 U0" },
            { "three: one put, two taken", LOADX LOADX LOADX, "D0 U0 U0" },
            { "a load nothing follows", LOADX, "d0" },
            { "two items, two registers", LOADX LOADY LOADX LOADY, "D0 D1 U0 U1" },
            { "a store between, unmarked", LOADX LA(1) "\tstb r3+0, r1\n" LOADX, "d0 d0" },
            { "a store between, marked: held from the store", LOADX LA(1) ST(0) LOADX, "d0 D0 U0" },
            { "a store to another item between", LOADX LA(2) "\tstb r3+0, r1\n" LOADX, "D0 U0" },
            { "a marked store of another item", LOADX LA(2) ST(1) LOADX LOADY, "D0 D1 U0 U1" },
            { "a store to an element of its record", LOADX LA(1) "\tadd r3, r3, r11\n\tstb r3+0, r1\n" LOADX, "d0 d0" },
            { "a store who knows where: nothing is held", LOADX LOADY "\tstb r7+0, r1\n" LOADX LOADY, "d0 d1 d0 d1" },
            { "a routine that stores nothing", LOADX "\tjal r31, cob_display_nl\n" LOADX, "D0 U0" },
            { "a routine nobody lists", LOADX "\tjal r31, cob_accept\n" LOADX, "d0 d0" },
            { "memcpy to the item", LOADX LA(1) "\tjal r31, memcpy\n" LOADX, "d0 d0" },
            { "memcpy to another", LOADX LOADY LA(2) "\tjal r31, memcpy\n" LOADX LOADY, "D0 d1 U0 d1" },
            { "a paragraph performed", LOADX "\tjal r0, .Lp0_3\n" LOADX, "d0 d0" },
            { "a jump through a register", LOADX "\tjalr r0, r1, 0\n" LOADX, "d0 d0" },
            { "a branch over nothing that matters", LOADX "\tbeq r1, r0, .L1\n\taddi r1, r1, 1\n.L1:\n" LOADX, "D0 U0" },
            { "a branch over a store to it: not held by the one way", LOADX "\tbeq r1, r0, .L1\n" LA(1) "\tstb r3+0, r1\n.L1:\n" LOADX, "d0 d0" },
            { "a branch over a marked store: held by both ways, put there by either", LOADX "\tbeq r1, r0, .L1\n" LA(1) ST(0) ".L1:\n" LOADX, "D0 D0 U0" },
            { "loaded on each of two ways, then joined", "\tbeq r1, r0, .L8\n" LOADX "\tjal r0, .L9\n.L8:\n" LOADX ".L9:\n" LOADX, "D0 D0 U0" },
            { "loaded on one of two ways only", "\tbeq r1, r0, .L8\n" LOADX ".L8:\n" LOADX, "d0 d0" },
            { "two ways, a different item in the register by each", "\tbeq r1, r0, .L8\n" LOADX "\tjal r0, .L9\n.L8:\n" LOADY ".L9:\n" LOADX, "d0 d0 d0" },
            { "the top of a loop: what was held coming in is not known to be held coming round", LOADX ".L3:\n" LOADX "\tbne r1, r0, .L3\n", "d0 d0" },
            { "... inside one pass it is", ".L3:\n" LOADX LOADX "\tbne r1, r0, .L3\n", "D0 U0" },
            { "a label nothing here jumps to", LOADX ".L6:\n" LOADX, "d0 d0" },
            { "a profile's label", LOADX "__ln_12_3:\n" LOADX, "D0 U0" },
            { "a loop that owns the register", LOADX "#@K< 1\n\taddi r1, r1, 1\n#@K>\n" LOADX, "d0 d0" },
            { "... owns another", LOADX "#@K< 2\n\taddi r1, r1, 1\n#@K>\n" LOADX, "D0 U0" },
            { "... inside it, the registers it does not own", "#@K< 1\n" LOADX LOADX "#@K>\n", "D1 U1" },
            { "... all four: nothing is held inside", "#@K< 15\n" LOADX LOADX "#@K>\n", "- -" },
            { "... one inside another, and after both", "#@K< 1\n#@K< 2\n" LOADX LOADX "#@K>\n" LOADX "#@K>\n" LOADX, "D2 U2 U2 U2" },      /* (held in a register neither owns: it stays held) */
            { "... held in what an inner loop then owns", "#@K< 1\n" LOADX "#@K< 2\n" LOADX "#@K>\n" LOADX "#@K>\n", "d1 D2 U2" },      /* (lost at the inner loop, loaded again into one neither owns) */
            { "five items: the one wanted longest ago gives up its register", LOADX LOADY LA(3) LD(2) LA(4) LD(3) LA(5) LD(4) LOADY LOADX, "d0 D1 d2 d3 d0 U1 d2" },
            { "... an item just taken from is not the one", LOADX LOADY LA(3) LD(2) LA(4) LD(3) LOADX LA(5) LD(4) LOADX LOADY, "D0 d1 d2 d3 U0 d1 U0 d2" },
            { "a load's mark on another item's address", LA(2) LD(0) LOADX, "- d0" },
            { "a store's mark that does not hold: the item is not held after it", LOADX LA(2) ST(0) LOADX, "d0 - d0" },
            { "a store's mark with a label in it", LOADX LA(1) "#@S 0 0 r1 r3\n\tstb r3+1, r1\n.L2:\n\tstb r3+0, r1\n#@.\n" LOADX, "d0 - d0" },
            { "DISPLAY digits", LA(7) "#@L 6 0 r1 r3\n\tldbu r2, r3+0\n\tandi r2, r2, 15\n\tadd r1, r2, r0\n\tslli r2, r1, 3\n\tslli r1, r1, 1\n\tadd r1, r1, r2\n\tldbu r2, r3+1\n\tandi r2, r2, 15\n\tadd r1, r1, r2\n#@.\n"
                                LA(7) "#@L 6 0 r1 r3\n\tldbu r2, r3+0\n\tandi r2, r2, 15\n\tadd r1, r2, r0\n#@.\n", "D0 U0" },
            { "... a digit stored between", LA(7) "#@L 6 0 r1 r3\n\tldbu r1, r3+0\n#@.\n" LA(7) "\tstb r3+1, r2\n" LA(7) "#@L 6 0 r1 r3\n\tldbu r1, r3+0\n#@.\n", "d0 d0" },
            { "a mark for an item there is none of", LA(1) "#@L 77 0 r1 r3\n\tldbu r1, r3+0\n#@.\n", "-" },
        };
        for (size_t c = 0; c < sizeof ac / sizeof *ac; c++) {
            int nl = load(ac[c].code);
            LrAvail av; memset(&av, 0, sizeof av);
            av.use = calloc((size_t)nl + 1, 1); av.def = calloc((size_t)nl + 1, 1); av.useful = calloc((size_t)nl + 1, 1);
            lr_scan(0, nl, NULL, 0, NULL, &av);
            char got[256]; int g = 0;
            for (int i = 0; i < nl; i++) {
                if (strncmp(g_asm[i], "#@L", 3) && strncmp(g_asm[i], "#@S", 3)) continue;
                if (g) got[g++] = ' ';
                if (av.use[i]) g += sprintf(got + g, "U%d", av.use[i] - 1);
                else if (av.def[i]) g += sprintf(got + g, "%c%d", av.useful[i] ? 'D' : 'd', av.def[i] - 1);
                else got[g++] = '-';
            }
            got[g] = 0;
            n++;
            if (strcmp(got, ac[c].want)) { printf("FAIL held, %s: [%s], want [%s]\n", ac[c].name, got, ac[c].want); bad++; }
            free(av.use); free(av.def); free(av.useful);
        }
    }
    printf("loopreg_test: %d checks, %d failed\n", n, bad);
    return bad != 0;
}
