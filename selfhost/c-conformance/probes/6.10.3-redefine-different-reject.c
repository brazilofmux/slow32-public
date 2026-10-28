// 6.10.3p2  object-like macro redefined with a different list
// expect: reject
#define OBJ 1
#define OBJ 2
int main(void) { return OBJ; }
