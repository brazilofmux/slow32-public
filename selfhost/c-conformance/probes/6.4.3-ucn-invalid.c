// 6.4.3p2  a UCN shall not name a basic-character-set character (here 'A') in an identifier
// expect: reject
int main(void) { int \u0041x = 1; return \u0041x; }
