/* More functions than the parser's table held: a fixed 16384, past which
 * every declaration was dropped without a word and a call to one was
 * "undeclared" (tables.h, TAB_PS_FUNCS).  17,000 prototypes, made by
 * pasting; the biggest expansion is also far past the old 8KB of
 * pp_exp. */
#define F(n) int f##n(void);
#define F10(n) F(n##0) F(n##1) F(n##2) F(n##3) F(n##4) F(n##5) F(n##6) F(n##7) F(n##8) F(n##9)
#define F100(n) F10(n##0) F10(n##1) F10(n##2) F10(n##3) F10(n##4) F10(n##5) F10(n##6) F10(n##7) F10(n##8) F10(n##9)
#define F1000(n) F100(n##0) F100(n##1) F100(n##2) F100(n##3) F100(n##4) F100(n##5) F100(n##6) F100(n##7) F100(n##8) F100(n##9)
#define F10000(n) F1000(n##0) F1000(n##1) F1000(n##2) F1000(n##3) F1000(n##4) F1000(n##5) F1000(n##6) F1000(n##7) F1000(n##8) F1000(n##9)
F10000(a)
F1000(b0) F1000(b1) F1000(b2) F1000(b3) F1000(b4) F1000(b5) F1000(b6)
int fb6999(void) { return 7; }
int main(void) { return fb6999() == 7 ? 0 : 1; }
