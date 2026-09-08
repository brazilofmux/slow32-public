/* GitHub issue 46: two initialized file-scope definitions are a
 * redefinition.  run-tests.sh expects gen1_cc to reject this; it is
 * not in the passing-compile list. */
int x = 1;
int x = 2;
int main(void) { return x; }
