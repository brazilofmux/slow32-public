/* String literals and char-array initializers in designated positions.
 * Exercises:
 *   - .tag = "string" filling a char-array struct member at file scope
 *     (single global) and at automatic scope (local struct).
 *   - .tag = { 'a','b',0,... } char-literal brace-list at file scope
 *     (regression test for ISSUES.md #52).
 *   - .short_tag = { [i]=ch, ... } sparse char-literal byte writes at
 *     file scope and automatic scope.
 *   - Designated int-array element writes at static scope.
 *   - Mixed string + designator forms inside an array-of-structs. */

struct entry {
    int  id;
    char tag[8];
    char short_tag[4];
};

struct numeric_entry {
    int id;
    int xs[4];
};

static struct entry g_one = {
    .id        = 1,
    .tag       = "alpha",
    .short_tag = "ab"
};

static struct entry g_two = {
    .id        = 2,
    .tag       = { 'b','e','t','a', 0, 0, 0, 0 },
    .short_tag = { 'c','d','e', 0 }
};

static struct entry g_three = {
    .id        = 3,
    .tag       = { [0]='g', [2]='m', [4]='a', [6]='!' },
    .short_tag = { [3]='Z', [0]='A' }
};

static struct entry g_entries[3] = {
    { .id = 100, .tag = "first",  .short_tag = "fa"  },
    { .id = 200, .tag = "second", .short_tag = { 'A', 'B', 0, 0 } },
    { .id = 300, .tag = { [0]='X', [3]='Y' }, .short_tag = { [2]='!' } }
};

static struct numeric_entry g_nums[3] = {
    { .id = 10, .xs = { 1, 2, 3, 4 } },
    { .id = 20, .xs = { [2] = 30, [0] = 10 } },
    { .id = 30, .xs = { [3] = 99 } }
};

static int score_entry(struct entry *e) {
    int i;
    int s = e->id * 1000;
    for (i = 0; i < 8; i++) {
        s = s + (int)e->tag[i] * (i + 1);
    }
    for (i = 0; i < 4; i++) {
        s = s + (int)e->short_tag[i] * (i + 2);
    }
    return s;
}

static int score_nums(struct numeric_entry *e) {
    int i;
    int s = e->id;
    for (i = 0; i < 4; i++) {
        s = s + e->xs[i] * (i + 1);
    }
    return s;
}

int main(void) {
    struct entry local = {
        .id        = 9,
        .tag       = "local!!",
        .short_tag = { [3] = 'Z', [0] = 'A' }
    };
    int acc = 0;
    int i;
    acc = acc * 17 + score_entry(&g_one);
    acc = acc * 17 + score_entry(&g_two);
    acc = acc * 17 + score_entry(&g_three);
    acc = acc * 17 + score_entry(&local);
    for (i = 0; i < 3; i++) {
        acc = acc * 17 + score_entry(&g_entries[i]);
        acc = acc * 17 + score_nums(&g_nums[i]);
    }
    return acc & 0xff;
}
