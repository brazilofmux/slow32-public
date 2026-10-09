/* A hex escape takes every hex digit that follows it (C90 6.1.3.4), so
 * "\x0041" is the one byte 'A'; the lexer used to stop after two digits
 * and make it 0x00 '4' '1'.  A value a char cannot hold ("\xC3\xA4b",
 * "\777") is a constraint violation and is now an error, as under clang
 * and gcc, where it used to be read as two digits plus a letter.  And a
 * char constant is the byte read as a (signed) char: '\xff' is -1, as
 * under clang, not the 255 it was.  Found when libutf's harness compiled
 * under stage08 cc and not under clang (selfhost ISSUES-83). */
static int len(const char *s) { int n = 0; while (s[n]) n++; return n; }

int main(void) {
    const char *a = "\x0041";
    const char *b = "\x41" "b";
    const char *c = "\xC3\xA4" "b";
    const char *d = "\101\60";
    if (len(a) != 1 || a[0] != 'A') return 1;
    if (len(b) != 2 || b[0] != 'A' || b[1] != 'b') return 2;
    if (len(c) != 3 || (c[0] & 255) != 0xC3 || (c[1] & 255) != 0xA4 || c[2] != 'b') return 3;
    if (len(d) != 2 || d[0] != 'A' || d[1] != '0') return 4;
    if ('\x7f' != 127 || '\xff' != (char)255 || '\0' != 0 || '\12' != 10) return 5;
    if (len("\x00000041") != 1 || "\x00000041"[0] != 'A') return 6;   /* leading zeros are digits too */
    return 0;
}
