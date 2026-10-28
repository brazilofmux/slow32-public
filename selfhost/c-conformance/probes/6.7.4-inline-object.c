// 6.7.4p1  function specifiers may be used only in declarations of functions
// expect: reject
inline int x = 1;
int main(void) { return x; }
