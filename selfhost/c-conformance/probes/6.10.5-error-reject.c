// 6.10.5p1  #error produces a diagnostic (and translation fails)
// expect: reject
#error this configuration is not supported
int main(void) { return 0; }
