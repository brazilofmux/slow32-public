// 6.2.5p19  void is an incomplete type that cannot be completed: no object of type void
// expect: reject
void v;
int main(void) { return 0; }
