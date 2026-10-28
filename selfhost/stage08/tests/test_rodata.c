/* What the program must not write goes in .rodata, which every engine
 * maps unwritable: string literals, jump tables, and globals whose object
 * is itself const.  It was all .data, so a write through a cast-away
 * const landed silently where clang's build faults (selfhost ISSUES-99).
 * The linker brackets .rodata with __rodata_start/__rodata_end, so the
 * program checks where each object landed -- and writes every object
 * that must stay writable, which would fault if it had been misplaced.
 * The harness also checks that the jump table follows .rodata. */
#include <stdio.h>

extern char __rodata_start[];
extern char __rodata_end[];

static int ro(const void *p) {
    const char *c = (const char *)p;
    return c >= __rodata_start && c < __rodata_end;
}

const int ci = 14;                      /* const object */
const int cz;                           /* const, zero-filled */
static const int tab[] = {1, 2, 3};     /* const array */
const char *const cpc = "pc";           /* const pointer to const */
const char *pc = "p";                   /* pointer to const: writable */
int plain = 5;                          /* writable */
const int m1 = 1, m2 = 2;               /* both const */
const char *q1 = "q1", *const q2 = "q2";/* q1 writable, q2 const */
const struct pt { int x; int y; } origin = {3, 4};
static struct { int a; const int b; } mixed = {1, 2};  /* writable: a member's const is not the struct's */
const char *const names[] = {"zero", "one"};   /* const array of const pointers */
const char *words[] = {"w0", "w1"};            /* writable array */

static int fail;

static void want(const char *what, int got, int expect) {
    if (got != expect) {
        printf("FAIL %s: %s, expected %s\n", what,
               got ? "in .rodata" : "writable", expect ? "in .rodata" : "writable");
        fail = 1;
    }
}

static int pick(int k) {
    switch (k) {                        /* dense: a jump table */
    case 0: return 11;
    case 1: return 22;
    case 2: return 33;
    case 3: return 44;
    case 4: return 55;
    case 5: return 66;
    case 6: return 77;
    case 7: return 88;
    default: return 0;
    }
}

int main(void) {
    want("ci", ro(&ci), 1);
    want("cz", ro(&cz), 1);
    want("tab", ro(tab), 1);
    want("cpc", ro(&cpc), 1);
    want("literal", ro(cpc), 1);
    want("pc", ro(&pc), 0);
    want("plain", ro(&plain), 0);
    want("m1", ro(&m1), 1);
    want("m2", ro(&m2), 1);
    want("q1", ro(&q1), 0);
    want("q2", ro(&q2), 1);
    want("origin", ro(&origin), 1);
    want("mixed", ro(&mixed), 0);
    want("names", ro(names), 1);
    want("words", ro(words), 0);
    want("words[0] literal", ro(words[0]), 1);

    /* The writable ones are written; a misplaced one would fault here. */
    pc = "p2";
    plain = 6;
    q1 = "q1b";
    mixed.a = 7;
    words[1] = "w1b";

    if (ci + cz + tab[2] + m1 + m2 + origin.y + mixed.a + mixed.b + plain != 14 + 0 + 3 + 1 + 2 + 4 + 7 + 2 + 6) {
        printf("FAIL values\n");
        fail = 1;
    }
    if (pick(3) + pick(7) + pick(9) != 132) {
        printf("FAIL switch\n");
        fail = 1;
    }
    if (pc[1] != '2' || q1[2] != 'b' || words[1][2] != 'b' || names[1][0] != 'o') {
        printf("FAIL strings\n");
        fail = 1;
    }
    if (!fail) printf("rodata ok\n");
    return fail;
}
