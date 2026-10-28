// 6.5.16p2/6.7.3  assigning to a const-qualified file-scope object
// expect: reject
const int g = 5;
int main(void) { g = 6; return g; }
