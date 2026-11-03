/* lex_src grows (c_lexer.rl lex_src_room), and what points into it
 * follows: the scanner's pointers and the parser's lexer marks.
 * run-tests compiles this with S12CC_SRCMOVE=1, which moves the buffer
 * at every request for room and fills the old one with garbage, so a
 * pointer left behind reads garbage here.  It covers: expansions under
 * a lexer mark (a static initializer is peeked at first), an #include
 * spliced in (this file, once), #if with function-like macros (their
 * arguments go past lex_len, the expression parked at the top), and a
 * long expansion. */
#ifndef SRC_MOVES_INNER
#define SRC_MOVES_INNER
#define SEVEN (3 + 4)
#define SUM10 (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10)
#define TWICE(x) ((x) + (x))
#define PICK(a, b, c) ((a) * 100 + (b) * 10 + (c))
#define L4(x) x x x x
#define L16(x) L4(L4(x))
static int a = SEVEN;
static int b = SUM10;
static int c = -SEVEN;
static int d = TWICE(SUM10);
static int e[3] = { SEVEN, TWICE(SEVEN), PICK(1, 2, 3) };
#if TWICE(PICK(1, 2, 3)) == 246 && SUM10 == 55
static int f = 1;
#else
static int f = 0;
#endif
#include "test_src_moves.c"
int main(void) {
    int t;
    t = 0 L16(+ SEVEN) L16(+ TWICE(1));
    if (a != 7 || b != 55 || c != -7 || d != 110) return 1;
    if (e[0] != 7 || e[1] != 14 || e[2] != 123) return 2;
    if (f != 1 || g != 42) return 3;
    if (t != 16 * 7 + 16 * 2) return 4;
    return 0;
}
#else
static int g = PICK(0, 4, 2);
#endif
