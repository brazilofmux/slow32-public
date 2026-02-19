/* Test goto + labels: forward, backward, in switch */

int test_forward(void) {
    int x;
    x = 0;
    goto skip;
    x = 99;
skip:
    if (x != 0) return 1;
    return 0;
}

int test_backward(void) {
    int i;
    int sum;
    i = 0;
    sum = 0;
again:
    if (i >= 5) goto done;
    sum = sum + i;
    i = i + 1;
    goto again;
done:
    if (sum != 10) return 1;
    return 0;
}

int test_goto_in_switch(void) {
    int x;
    int r;
    x = 2;
    r = 0;
    switch (x) {
    case 1:
        r = 10;
        break;
    case 2:
        goto special;
    case 3:
        r = 30;
        break;
    }
    goto end;
special:
    r = 42;
end:
    if (r != 42) return 1;
    return 0;
}

int test_label_stmt(void) {
    int x;
    x = 0;
    goto lbl;
    return 1;
lbl: x = 42;
    if (x != 42) return 2;
    return 0;
}

int main(void) {
    int rc;
    rc = test_forward();
    if (rc) return rc;
    rc = test_backward();
    if (rc) return rc + 10;
    rc = test_goto_in_switch();
    if (rc) return rc + 20;
    rc = test_label_stmt();
    if (rc) return rc + 30;
    return 0;
}
