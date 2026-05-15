/* String literals and char-array initializers in designated positions.
 * Exercises:
 *   - .tag = "string" filling a char-array struct member at file scope
 *     (single global) and at automatic scope (local struct).
 *   - .short_tag = { [i]=ch, ... } sparse char-array byte writes at
 *     automatic scope (designated char literals).
 *   - Designated int-array element writes at static scope.
 *
 * Brace-list with character literals for a static char-array member
 * (e.g. .tag = { 'a','b',0 }) is a separate frontend gap (tracked in
 * ISSUES.md); use the string-literal form or integer constants in
 * static scope. */

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
    .tag       = "beta",
    .short_tag = "cde"
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
    acc = acc * 17 + score_entry(&local);
    for (i = 0; i < 3; i++) {
        acc = acc * 17 + score_nums(&g_nums[i]);
    }
    return acc & 0xff;
}
