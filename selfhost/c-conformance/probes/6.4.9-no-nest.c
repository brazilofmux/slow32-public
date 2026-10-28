// 6.4.9p1  /* comments do not nest: the first */ ends it, leaving a stray */
// expect: reject
int main(void) { /* outer /* inner */ still comment? */ return 0; }
