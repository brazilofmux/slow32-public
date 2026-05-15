/* Block-scope compound literals -- scalar and struct forms.  Each
 * literal lowers to a hidden automatic local; address-taking through
 * the literal must work, and ordered side effects in the init list
 * must execute in init order.
 *
 * Struct-by-value passing is intentionally avoided here -- the AAPCS64
 * struct-by-value path is a separate concern (see ISSUES.md).  All
 * struct accesses go through a pointer. */

struct pair {
    int a;
    int b;
};

static int sum_pair_p(struct pair *p) { return p->a * 3 + p->b * 5; }
static int diff_pair_p(struct pair *p) { return p->a - p->b; }

int main(void) {
    int acc = 0;
    int i;
    for (i = 0; i < 12; i++) {
        struct pair *p = &(struct pair){ i, i * i };
        int v          = (int){ i + 1 };
        int *q         = &(int){ i * 7 + 3 };

        acc = acc * 3 + sum_pair_p(p);
        acc = acc * 3 + diff_pair_p(&(struct pair){ i + 2, i - 1 });
        acc = acc * 3 + v + *q;
        *q  = *q + 1;
        acc = acc + *q;
    }
    return acc & 0xff;
}
