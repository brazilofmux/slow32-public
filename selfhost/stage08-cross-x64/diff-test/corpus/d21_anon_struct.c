/* C11 anonymous struct member: layout matches the named-struct version,
 * and member access through the outer struct must hit the inner offsets.
 * Mixes flat initialization, braced initialization, pointer mutation,
 * and a loop that touches every field. */

struct node {
    int tag;
    struct {
        int hi;
        int lo;
    };
    int extra;
};

static struct node g_flat   = { 1, 2, 3, 4 };
static struct node g_braced = { 5, { 6, 7 }, 8 };

static int churn(struct node *n, int iters) {
    int acc = 0;
    int i;
    for (i = 0; i < iters; i++) {
        n->hi    = n->hi + n->lo;
        n->lo    = n->lo ^ 0x55;
        n->extra = n->extra + n->tag - n->hi + n->lo;
        n->tag   = (n->tag + 1) & 0x3f;
        acc = acc + n->hi - n->lo + n->extra + n->tag;
    }
    return acc;
}

int main(void) {
    struct node local = { 10, 20, 30, 40 };
    int acc = 0;
    acc = acc * 17 + churn(&local, 25);
    acc = acc * 17 + churn(&g_flat, 25);
    acc = acc * 17 + churn(&g_braced, 25);
    acc = acc * 17 + (g_flat.hi + g_flat.lo + g_flat.extra + g_flat.tag);
    acc = acc * 17 + (g_braced.hi + g_braced.lo + g_braced.extra + g_braced.tag);
    return acc & 0xff;
}
