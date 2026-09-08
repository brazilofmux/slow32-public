/* GitHub issue 46: `int x = 0` is a completed definition.  ps_ginit==0
 * is also the tentative default, so a has-init flag is required;
 * without it this compiles and the second initializer wins. */
int x = 0;
int x = 7;
int main(void) { return x; }
