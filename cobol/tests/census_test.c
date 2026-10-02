/* host test for the census's reading of what is done with an item's
 * bytes (src/cobc/symtab.h: cen_formed and the rest).
 *
 * An item is written the machine's way only if every use of it is a use
 * of its number (src/cobc/native.h).  The rule that decides is "an
 * address formed is accounted for by a load or store of the item's value
 * through that register, or by a routine known to take it as a number;
 * anything else pins the item".  The cases it must refuse -- the register
 * used for something else in between, the address formed twice and only
 * one of them a load, a call that takes a different register -- the
 * compiler does not write today, so no COBOL program reaches them (the
 * generator's mutants "untouched" and "reg" survive every program).
 * Here the rule is given the events directly.
 *
 * The compiler is one translation unit; this includes it. */
#define main s32_cobc_main
#include "../src/s32-cobc.c"
#undef main

static int n, bad;
static Sym *X, *Y;

static void begin(void)
{
    g_nasm = 0; g_cen_npend = 0; g_cen_nnamed = 0; g_cen_naddr = 0; g_cen_hold = 0; g_noemit = 0;
    cen_of(X)->pins = 0; cen_of(Y)->pins = 0;
    snprintf(g_cur_stmt, sizeof g_cur_stmt, "MOVE");
}
/* the item's address into reg, as emit_item_addr leaves it */
static void form(Sym *s, const char *reg)
{
    emit("\tlui %s, %%hi(ws0_1)", reg); emit("\taddi %s, %s, %%lo(ws0_1)", reg, reg);
    cen_formed(s, reg);
}
static void check(const char *name, Sym *s, int want)
{
    cen_stmt_end();
    n++;
    int got = cen_of(s)->pins != 0;
    if (got != want) { printf("FAIL %s: %s %s, want %s\n", name, s->name, got ? "pinned" : "free", want ? "pinned" : "free"); bad++; }
}
static const char *why(Sym *s)
{
    for (int k = 0; k < 64; k++) if (cen_of(s)->pins >> k & 1) return g_cen_why[k];
    return "";
}

int main(void)
{
    g_cen_on = 1;
    X = sym_new(); snprintf(X->name, sizeof X->name, "x");
    Y = sym_new(); snprintf(Y->name, sizeof Y->name, "y");

    begin(); form(X, "r3"); cen_valued(X, "r3"); check("an address, then a load through it", X, 0);
    begin(); form(X, "r3"); emit("\taddi r1, r0, 5"); emit("\tldw r2, sp+108"); cen_valued(X, "r3");
    check("other registers used in between", X, 0);
    begin(); form(X, "r3"); emit("\tadd r3, r3, r11"); cen_valued(X, "r3"); check("the register moved on before the load", X, 1);
    begin(); form(X, "r3"); emit("\tldbu r1, r3+0"); cen_valued(X, "r3"); check("a byte read through it before the load", X, 1);
    begin(); form(X, "r3"); emit("\tjal r31, .L4"); emit("\tadd r30, r31, r0"); emit("\tldw r13, sp+4"); cen_valued(X, "r3");
    check("r31, r30 and r13 are not r3", X, 0);
    begin(); form(X, "r3"); emit("#@L 0 0 r1 r3"); cen_valued(X, "r3"); check("a mark naming the register is not code", X, 0);
    begin(); form(X, "r3"); cen_valued(X, "r4"); check("a load through another register", X, 1);
    begin(); form(X, "r3"); cen_valued(Y, "r3"); check("a load of another item", X, 1);
    begin(); form(X, "r3"); emit("\tldbu r1, r3+0"); form(X, "r3"); cen_valued(X, "r3");
    check("formed twice, the first for its bytes", X, 1);
    begin(); form(X, "r3"); cen_valued(X, "r3"); form(X, "r3"); cen_valued(X, "r3"); check("formed twice, loaded twice", X, 0);
    begin(); form(X, "r3"); check("an address nothing accounts for", X, 1);
    n++; if (strcmp(why(X), "inline/MOVE")) { printf("FAIL the reason: %s, want inline/MOVE\n", why(X)); bad++; }

    begin(); form(X, "r3"); emit("\tlui r4, 1"); cen_called("cob_push"); check("a routine that takes the number", X, 0);
    begin(); form(X, "r3"); cen_called("cob_accept"); check("a routine nobody lists", X, 1);
    n++; if (strcmp(why(X), "cob_accept")) { printf("FAIL the reason: %s, want cob_accept\n", why(X)); bad++; }
    begin(); form(X, "r4"); cen_called("cob_push"); check("... the number's routine, the address not its first argument", X, 1);
    begin(); form(X, "r3"); emit("\tldw r3, sp+72"); cen_called("cob_push"); check("... its first argument loaded with something else since", X, 1);
    begin(); form(X, "r5"); form(Y, "r3"); cen_called("cob_push");
    check("two addresses, one the routine's: that one", Y, 0);
    n++; if (!cen_of(X)->pins) { printf("FAIL two addresses, one the routine's: the other is free\n"); bad++; }
    begin(); form(X, "r3"); form(Y, "r3"); cen_called("cob_push"); check("the same register formed again: the earlier one is not the argument", X, 1);
    begin(); form(X, "r3"); form(Y, "r4"); cen_called("memcpy"); check("a routine that takes bytes pins every address in hand", X, 1);
    n++; if (!cen_of(Y)->pins) { printf("FAIL a routine that takes bytes: the second address is free\n"); bad++; }

    begin(); g_cen_hold++; form(X, "r3"); g_cen_hold--; check("an address the one forming it answers for", X, 0);
    begin(); form(X, "r3"); cen_bless(X); check("blessed by the one emitting", X, 0);
    begin(); form(X, "r3"); emit("\tldbu r1, r3+0"); form(X, "r5"); cen_bless(X); check("... the latest address only", X, 1);
    begin(); form(X, "r3"); g_nasm = 0; cen_valued(X, "r3"); check("the code cut away since the address was formed", X, 1);
    begin(); g_noemit = 1; form(X, "r3"); cen_valued(X, "r3"); g_noemit = 0; check("while scanning ahead: nothing to read, nothing held against it", X, 0);

    /* an element of a table: its index added to the address, which is whole after that */
    begin(); form(X, "r3"); emit("\tadd r3, r3, r11"); cen_reformed(X, "r3"); cen_valued(X, "r3");
    check("an element's address, its index added, then a load", X, 0);
    begin(); form(X, "r3"); emit("\tadd r3, r3, r11"); cen_reformed(X, "r3"); emit("\tldbu r1, r3+0"); cen_valued(X, "r3");
    check("... a byte read through it after that", X, 1);
    begin(); form(X, "r3"); emit("\tadd r3, r3, r11"); cen_reformed(X, "r4"); cen_valued(X, "r3");
    check("... the index added to another register's address", X, 1);
    /* an address parked in the frame and brought back as an argument */
    begin(); form(X, "r1"); emit("\tstw sp+72, r1"); emit("\taddi r1, r0, 5"); emit("\tldw r3, sp+72"); cen_moved(X, "r3");
    cen_called("cob_push"); check("an address back from its slot, to a routine that takes the number", X, 0);
    begin(); form(X, "r1"); emit("\tstw sp+72, r1"); emit("\tldw r4, sp+72"); cen_moved(X, "r4");
    cen_called("cob_push"); check("... back in a register that is not the routine's first", X, 1);
    begin(); form(X, "r1"); emit("\tstw sp+72, r1"); emit("\tldw r3, sp+72"); cen_moved(X, "r3"); emit("\taddi r3, r3, 1");
    cen_called("cob_push"); check("... and moved on before the call", X, 1);
    begin(); form(X, "r1"); emit("\tstw sp+72, r1"); emit("\tldw r3, sp+72"); cen_moved(X, "r3");
    cen_called("memcpy"); check("... to a routine that takes bytes", X, 1);

    /* every name owes an address */
    g_ntok = 100;
    begin(); g_tp = 10; cen_ref(X); cen_stmt_owed(0, 0, 1); check("named, its address never formed", X, 1);
    n++; if (strcmp(why(X), "noaddr/MOVE")) { printf("FAIL the reason: %s, want noaddr/MOVE\n", why(X)); bad++; }
    begin(); g_tp = 11; cen_ref(X); form(X, "r3"); cen_valued(X, "r3"); cen_stmt_owed(0, 0, 1); check("named once, formed once", X, 0);
    begin(); g_tp = 12; cen_ref(X); g_tp = 13; cen_ref(X); form(X, "r3"); cen_valued(X, "r3"); cen_stmt_owed(0, 0, 1);
    check("named twice, formed once", X, 1);
    begin(); g_tp = 14; cen_ref(X); form(X, "r3"); cen_valued(X, "r3"); form(X, "r3"); cen_valued(X, "r3"); cen_stmt_owed(0, 0, 1);
    check("named once, formed twice (read, then stored)", X, 0);
    begin(); g_tp = 15; cen_ref(X); cen_ref(X); form(X, "r3"); cen_valued(X, "r3"); cen_stmt_owed(0, 0, 1);
    check("the same token read twice is one name", X, 0);
    begin(); g_tp = 16; cen_ref(X); g_noemit = 1; form(X, "r3"); g_noemit = 0; g_cen_npend = 0; cen_stmt_owed(0, 0, 1);
    check("an address formed only while scanning ahead is not one", X, 1);
    begin(); g_tp = 17; cen_ref(X); g_cen_hold++; form(X, "r3"); g_cen_hold--; cen_stmt_owed(0, 0, 1);
    check("an address formed and answered for is one", X, 0);
    begin(); g_tp = 18; cen_ref(Y); form(Y, "r3"); cen_valued(Y, "r3");
    { int n0 = g_cen_nnamed, a0 = g_cen_naddr; g_tp = 19; cen_ref(X); cen_stmt_owed(n0, a0, 0); }
    check("a statement inside another: its own names", X, 1);
    n++; if (cen_of(Y)->pins) { printf("FAIL a statement inside another: the outer one's item is pinned\n"); bad++; }

    begin(); g_tp = 20; cen_ref(X); form(X, "r3"); cen_valued(X, "r3");
    { int n0 = g_cen_nnamed, a0 = g_cen_naddr; g_tp = 21; cen_ref(X); cen_stmt_owed(n0, a0, 0); }
    check("... its name is not paid for by the outer one's address", X, 1);

    printf("census_test: %d checks, %d failed\n", n, bad);
    return bad != 0;
}
