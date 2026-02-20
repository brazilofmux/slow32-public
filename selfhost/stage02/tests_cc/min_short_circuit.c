/* Test short-circuit evaluation of && and ||.
 * A global counter tracks whether the RHS was evaluated. */
int g_count;

void bump(void) {
    g_count = g_count + 1;
}

int side_true(void) {
    bump();
    return 1;
}

int side_false(void) {
    bump();
    return 0;
}

int main(void) {
    int r;

    g_count = 0;

    /* && short-circuits on false LHS: RHS not evaluated */
    r = 0 && side_true();
    if (r != 0) return 1;
    if (g_count != 0) return 2;

    /* && evaluates RHS when LHS is true */
    r = 1 && side_true();
    if (r != 1) return 3;
    if (g_count != 1) return 4;

    /* || short-circuits on true LHS: RHS not evaluated */
    g_count = 0;
    r = 1 || side_false();
    if (r != 1) return 5;
    if (g_count != 0) return 6;

    /* || evaluates RHS when LHS is false */
    r = 0 || side_true();
    if (r != 1) return 7;
    if (g_count != 1) return 8;

    return 0;
}
