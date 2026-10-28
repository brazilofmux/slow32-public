// 6.7.9p2  no initializer may provide a value for an object not contained in the entity
// expect: reject
int a[2] = { 1, 2, 3 };
int main(void) { return a[0]; }
