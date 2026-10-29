// 6.7.9p11/6.5.16.1p1  a member of an automatic structure initialized from a pointer to another type
// expect: reject
struct s { long *p; };
int g;
int main(void) { struct s v = { &g }; return v.p != 0; }
