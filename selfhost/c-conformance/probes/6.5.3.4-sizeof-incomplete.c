// 6.5.3.4p1  sizeof may not be applied to an incomplete type
// expect: reject
struct Never;
int main(void) { return (int)sizeof(struct Never); }
