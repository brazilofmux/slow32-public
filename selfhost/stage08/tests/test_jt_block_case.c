/* A case label inside a plain block keeps the jump table.  SQLite's
 * sqlite3VdbeExec writes `case OP_ReopenIdx: { ...; goto x; case
 * OP_OpenRead: case OP_OpenWrite: { ... } }`, and the Duff check
 * refused a table for the whole 184-case switch because of it.  The
 * nested label is reached by fall-through from its block and from the
 * table, so it gets a trampoline; a label under a loop or an if still
 * declines.  The harness checks this file has a .LJT. */
static int reopen_count;

static int exec(int op, int have) {
    int r = 0;
    switch (op) {
    case 0: { r = 1; break; }
    case 1: { r = 2; break; }
    case 2: {
        reopen_count = reopen_count + 1;
        if (have) { r = 30; break; }
        /* not open: fall into the open cases */
    case 3:
    case 4: {
        r = r + 40 + op;
        break;
    }
    }
    case 5: { r = 5; break; }
    case 6: { r = 6;
    case 7:
        r = r + 70;
        break;
    }
    default: r = -1; break;
    }
    return r;
}

/* under an if: still a compare tree, still right */
static int guarded(int op, int c) {
    int r = 0;
    switch (op) {
    case 0: r = 1; break;
    case 1: if (c) { r = 10; case 2: r = r + 20; } break;
    case 3: r = 3; break;
    case 4: r = 4; break;
    case 5: r = 5; break;
    }
    return r;
}

int main(void) {
    if (exec(0, 0) != 1) return 1;
    if (exec(1, 0) != 2) return 2;
    if (exec(2, 1) != 30) return 3;
    if (exec(2, 0) != 42) return 4;      /* fell into case 3/4's block with op 2 */
    if (exec(3, 0) != 43) return 5;
    if (exec(4, 0) != 44) return 6;
    if (exec(5, 0) != 5) return 7;
    if (exec(6, 0) != 76) return 8;
    if (exec(7, 0) != 70) return 9;
    if (exec(9, 0) != -1) return 10;
    if (reopen_count != 2) return 11;
    if (guarded(0, 0) != 1) return 12;
    if (guarded(1, 1) != 30) return 13;
    if (guarded(1, 0) != 0) return 14;
    if (guarded(2, 0) != 20) return 15;
    if (guarded(5, 0) != 5) return 16;
    return 0;
}
