// 6.7.1p3  _Thread_local at block scope needs static or extern
// expect: reject
int main(void) { _Thread_local int x = 1; return x; }
