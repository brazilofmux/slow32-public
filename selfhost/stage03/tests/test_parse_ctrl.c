/* Test control flow: if/while/for/do-while/switch/break/continue */

int test_if(void) {
    int x;
    x = 0;
    if (1) x = 1;
    if (x != 1) return 1;
    if (0) x = 99;
    if (x != 1) return 2;
    /* if-else */
    if (1) x = 10; else x = 20;
    if (x != 10) return 3;
    if (0) x = 10; else x = 20;
    if (x != 20) return 4;
    /* nested if */
    if (1) {
        if (1) x = 42;
    }
    if (x != 42) return 5;
    return 0;
}

int test_while(void) {
    int i;
    int sum;
    i = 0;
    sum = 0;
    while (i < 10) {
        sum = sum + i;
        i = i + 1;
    }
    if (sum != 45) return 1;
    return 0;
}

int test_for(void) {
    int i;
    int sum;
    sum = 0;
    for (i = 1; i <= 10; i = i + 1) {
        sum = sum + i;
    }
    if (sum != 55) return 1;
    return 0;
}

int test_for_decl(void) {
    int sum;
    int i;
    sum = 0;
    /* for with init declaration */
    for (i = 0; i < 5; i = i + 1) {
        sum = sum + i;
    }
    if (sum != 10) return 1;
    return 0;
}

int test_do_while(void) {
    int i;
    int sum;
    i = 0;
    sum = 0;
    do {
        sum = sum + i;
        i = i + 1;
    } while (i < 10);
    if (sum != 45) return 1;
    /* do-while that runs exactly once */
    i = 0;
    do {
        i = i + 1;
    } while (0);
    if (i != 1) return 2;
    return 0;
}

int test_break(void) {
    int i;
    int sum;
    i = 0;
    sum = 0;
    while (1) {
        if (i >= 5) break;
        sum = sum + i;
        i = i + 1;
    }
    if (sum != 10) return 1;
    return 0;
}

int test_continue(void) {
    int i;
    int sum;
    sum = 0;
    for (i = 0; i < 10; i = i + 1) {
        if (i % 2 == 0) continue;
        sum = sum + i;
    }
    /* sum of odd numbers 1+3+5+7+9 = 25 */
    if (sum != 25) return 1;
    return 0;
}

int test_switch(void) {
    int x;
    int r;
    /* Basic switch */
    x = 2;
    r = 0;
    switch (x) {
    case 1:
        r = 10;
        break;
    case 2:
        r = 20;
        break;
    case 3:
        r = 30;
        break;
    default:
        r = 99;
        break;
    }
    if (r != 20) return 1;
    /* Default case */
    x = 99;
    switch (x) {
    case 1:
        r = 10;
        break;
    default:
        r = 42;
        break;
    }
    if (r != 42) return 2;
    /* Fall-through */
    x = 1;
    r = 0;
    switch (x) {
    case 1:
        r = r + 1;
    case 2:
        r = r + 10;
        break;
    case 3:
        r = r + 100;
        break;
    }
    if (r != 11) return 3;
    return 0;
}

int test_nested_loops(void) {
    int i;
    int j;
    int sum;
    sum = 0;
    for (i = 0; i < 3; i = i + 1) {
        for (j = 0; j < 3; j = j + 1) {
            sum = sum + 1;
        }
    }
    if (sum != 9) return 1;
    /* Break in inner loop only */
    sum = 0;
    for (i = 0; i < 3; i = i + 1) {
        for (j = 0; j < 10; j = j + 1) {
            if (j >= 2) break;
            sum = sum + 1;
        }
    }
    if (sum != 6) return 2;
    return 0;
}

int main(void) {
    int rc;
    rc = test_if();
    if (rc) return rc;
    rc = test_while();
    if (rc) return rc + 10;
    rc = test_for();
    if (rc) return rc + 20;
    rc = test_for_decl();
    if (rc) return rc + 30;
    rc = test_do_while();
    if (rc) return rc + 40;
    rc = test_break();
    if (rc) return rc + 50;
    rc = test_continue();
    if (rc) return rc + 60;
    rc = test_switch();
    if (rc) return rc + 70;
    rc = test_nested_loops();
    if (rc) return rc + 80;
    return 0;
}
