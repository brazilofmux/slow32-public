/* test_offsetof.c — offsetof() and __builtin_offsetof() tests
 *
 * Expected exit code: 0 (all pass)
 */

struct Point {
    int x;
    int y;
    int z;
};

struct Mixed {
    char a;
    int b;
    short c;
    char d;
};

struct Nested {
    int tag;
    struct Point pt;
    int flags;
};

int main(int argc, char **argv) {
    int fails;
    fails = 0;

    /* Simple struct: contiguous ints */
    if (offsetof(struct Point, x) != 0) fails = fails + 1;
    if (offsetof(struct Point, y) != 4) fails = fails + 2;
    if (offsetof(struct Point, z) != 8) fails = fails + 4;

    /* Mixed types with alignment */
    if (offsetof(struct Mixed, a) != 0) fails = fails + 8;
    if (offsetof(struct Mixed, b) != 4) fails = fails + 16;

    /* __builtin_offsetof form */
    if (__builtin_offsetof(struct Point, y) != 4) fails = fails + 32;
    if (__builtin_offsetof(struct Mixed, b) != 4) fails = fails + 64;

    /* Nested struct */
    if (offsetof(struct Nested, tag) != 0) fails = fails + 128;
    if (offsetof(struct Nested, pt) != 4) fails = fails + 256;
    if (offsetof(struct Nested, flags) != 16) fails = fails + 512;

    /* sizeof still works */
    if (sizeof(struct Point) != 12) fails = fails + 1024;

    return fails;
}
