// 6.10p2  directives: leading/inner blanks, tabs, comments inside, skipped-group junk
#include <stdio.h>
   #   define A 3
	#	define B /* comment */ 4 // trailing
#/* comment before name */define C 5
#if 0
#this is not a directive name at all
#else
#define D 6
#endif
int main(void) {
    printf("%d %d %d %d\n", A, B, C, D);
    return 0;
}
