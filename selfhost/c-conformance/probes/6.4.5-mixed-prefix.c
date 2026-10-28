// 6.4.5p2  adjacent literals with different encoding prefixes (u8 with L) shall not concatenate
// expect: reject
int main(void) { const void *p = u8"a" L"b"; return p != 0; }
