/* ISSUES-54: '-' in an aggregate initializer must be followed by a number. */
int k;
int n[2] = {-k, 3};
int main(void) { return 0; }
