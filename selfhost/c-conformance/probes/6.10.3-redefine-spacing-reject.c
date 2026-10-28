// 6.10.3p1,p2  separation counts: (1+2) and (1 + 2) are not identical lists
// expect: reject
#define OBJ (1+2)
#define OBJ (1 + 2)
int main(void) { return OBJ; }
