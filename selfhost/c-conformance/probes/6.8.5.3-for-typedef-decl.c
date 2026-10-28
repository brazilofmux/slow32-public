// 6.8.5p3  a typedef in a for clause-1 declares no object of auto/register class
// expect: reject
int main(void) {
    int n = 0;
    for (typedef int T; n < 3; n++) ;
    return n;
}
