// 6.8.5p3  a for declaration may only declare objects of storage class auto or register
// expect: reject
int main(void) {
    int n = 0;
    for (static int i = 0; i < 3; i++) n++;
    return n;
}
