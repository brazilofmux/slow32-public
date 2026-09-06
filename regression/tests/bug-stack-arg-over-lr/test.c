/* A function with no locals that forwards eight register arguments plus a
 * ninth on the stack stored the ninth at SP+0 -- over its own saved return
 * address -- and returned to address 0, restarting the program.  The
 * outgoing call frame was never materialised: PEI only walked functions with
 * stack objects, and the LR slot is not one (sqlite3_create_function,
 * 2026-09-06).  The wrapper must return here, and the ninth argument must
 * arrive. */
void debug_char(char c);
static void put(const char *s) { while (*s) debug_char(*s++); }
static void putn(int n) { char b[12]; int i = 0; if (n == 0) b[i++] = '0'; while (n > 0) { b[i++] = (char)('0' + n % 10); n /= 10; } while (i > 0) debug_char(b[--i]); }

__attribute__((noinline)) int nine(int a, int b, int c, int d, int e, int f, int g, int h, int i) {
    return a + b + c + d + e + f + g + h + i * 100;
}
__attribute__((noinline)) int wrap(int a, int b, int c, int d, int e, int f, int g, int h) {
    return nine(a, b, c, d, e, f, g, h, 7);   /* no locals: nothing but LR in the frame */
}
int main(void) {
    put("wrap="); putn(wrap(1, 2, 3, 4, 5, 6, 7, 8)); put("\n");
    put("back in main\n");
    return 0;
}
