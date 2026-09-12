/* GitHub issue 73: a spliced callee that contains a loop (or a switch)
 * must not disturb the caller's break/continue targets.  Zeroing the
 * loop depth for the callee body made its loop overwrite the caller's
 * depth-0 targets, so a caller `break` after the splice jumped to the
 * callee loop's exit: sqlite's balance() hung, and this file returned
 * 5000 where 0 was expected.  run-tests.sh compiles it at budget 200
 * and checks the statics were spliced (no out-of-line copy). */
struct bt;
struct cur { int state; int page; int ovf; struct cur *next; struct bt *bt; };
struct bt { struct cur *cursors; };

static int another(struct cur *c) {
    struct cur *o;
    for (o = c->bt->cursors; o; o = o->next) {
        if (o != c && o->state == 1 && o->page == c->page) return 11;
    }
    return 0;
}

static int deeper(struct cur *c, int *out) {
    int i, s = 0;
    for (i = 0; i < c->page + 3; i++) s += i * 7;
    *out = s;
    if (s > 100) return 5;
    return 0;
}

static int classify(int x) {
    switch (x) {
    case 0: return 10;
    case 1: return 20;
    default: return 30;
    }
}

int balance(struct cur *c) {
    int rc = 0, n = 0, t = 0;
    do {
        if (c->page > 5) break;
        else if (c->page == 0) {
            if (c->ovf && (rc = another(c)) == 0) {
                rc = deeper(c, &t);
                if (rc == 0) { c->page = c->page + 1; n++; }
            } else break;
        } else { c->page++; n++; }
    } while (rc == 0);
    return rc * 1000 + n * 10 + (t & 7);
}

int sw_in_loop(int lim) {
    int i, acc = 0;
    for (i = 0; i < lim; i++) {
        acc += classify(i);
        if (acc > 50) break;      /* must leave THIS loop */
        acc += 1;
    }
    return acc;
}

int main(void) {
    struct bt b; struct cur c1, c2, c3;
    b.cursors = &c1; c1.next = &c2; c2.next = &c3; c3.next = 0;
    c1.bt = &b; c2.bt = &b; c3.bt = &b;
    c3.state = 1; c3.page = 3; c3.ovf = 0;
    /* no overflow: break at once */
    c1.state = 0; c1.page = 0; c1.ovf = 0;
    if (balance(&c1) != 0) return 1;
    /* overflow, no other cursor on the page: deeper runs (t=21), page 1..6 */
    c1.page = 0; c1.ovf = 1; c2.state = 0; c2.page = 9;
    if (balance(&c1) != 65) return 2;
    /* overflow with a valid sibling on the same page: rc 11 */
    c1.page = 0; c1.ovf = 1; c2.state = 1; c2.page = 0;
    if (balance(&c1) != 11000) return 3;
    /* switch spliced into a loop body; the break after it is the caller's */
    if (sw_in_loop(10) != 62) return 4;
    if (sw_in_loop(2) != 32) return 5;
    return 0;
}
