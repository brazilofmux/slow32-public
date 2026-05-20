/* Test: goto labels inside switch (Duff's device pattern) */

int test_basic_duff(void) {
    int cs;
    int result;
    result = 0;
    cs = 2;
    switch (cs) {
    tr0:
        result = result + 100;
        goto done;
    tr1:
        result = result + 200;
        goto done;
    case 1:
        result = result + 1;
        goto tr0;
    case 2:
        result = result + 2;
        goto tr1;
    case 3:
        result = result + 3;
        goto tr0;
    }
done:
    /* cs=2: case 2 → result=2, goto tr1 → result=202 */
    if (result != 202) return 1;
    return 0;
}

int test_labels_before_cases(void) {
    int cs;
    int r;
    r = 0;
    cs = 1;
    switch (cs) {
    action_a:
        r = r + 10;
        goto out;
    action_b:
        r = r + 20;
        goto out;
    action_c:
        r = r + 30;
        goto out;
    case 1:
        r = r + 1;
        goto action_b;
    case 2:
        r = r + 2;
        goto action_a;
    case 3:
        r = r + 3;
        goto action_c;
    }
out:
    /* cs=1: case 1 → r=1, goto action_b → r=21 */
    if (r != 21) return 1;
    /* Try cs=3 */
    r = 0;
    cs = 3;
    switch (cs) {
    act_x:
        r = r + 10;
        goto out2;
    act_y:
        r = r + 20;
        goto out2;
    case 1:
        goto act_x;
    case 2:
        goto act_y;
    case 3:
        r = r + 3;
        goto act_y;
    }
out2:
    /* cs=3: case 3 → r=3, goto act_y → r=23 */
    if (r != 23) return 2;
    return 0;
}

/* Test inner switch inside outer switch with goto labels (like Ragel) */
int test_nested_switches(void) {
    int cs;
    int ch;
    int r;
    r = 0;
    cs = 1;
    ch = 65;
    switch (cs) {
    tr_add:
        r = r + 100;
        goto end;
    tr_sub:
        r = r - 50;
        goto end;
    case 1:
        switch (ch) {
        case 65: goto tr_add;
        case 66: goto tr_sub;
        }
        break;
    case 2:
        r = 999;
        break;
    }
end:
    /* cs=1, ch=65('A'): case 1 → inner switch → goto tr_add → r=100 */
    if (r != 100) return 1;
    return 0;
}

int main(void) {
    int rc;
    rc = test_basic_duff();
    if (rc) return rc;
    rc = test_labels_before_cases();
    if (rc) return rc + 10;
    rc = test_nested_switches();
    if (rc) return rc + 20;
    return 0;
}
