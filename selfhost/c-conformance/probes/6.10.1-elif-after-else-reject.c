// 6.10p1  syntax: #elif may not follow #else in one if-section
// expect: reject
#if 0
#else
#elif 1
#endif
int main(void) { return 0; }
