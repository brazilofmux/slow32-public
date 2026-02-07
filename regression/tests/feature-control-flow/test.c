#include <stdio.h>

/* Side-effect counter for short-circuit tests */
int counter;
int inc_true(void) { counter++; return 1; }
int inc_false(void) { counter++; return 0; }

int main() {
    int i, j, sum;

    /* Nested loops with break */
    sum = 0;
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 10; j++) {
            if (j == 3) break;
            sum++;
        }
    }
    printf("%s: nested break\n", sum == 15 ? "PASS" : "FAIL");

    /* Nested loops with continue */
    sum = 0;
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 5; j++) {
            if (j == 2) continue;
            sum++;
        }
    }
    printf("%s: nested continue\n", sum == 20 ? "PASS" : "FAIL");

    /* Break and continue together */
    sum = 0;
    for (i = 0; i < 10; i++) {
        if (i % 2 == 0) continue;
        if (i >= 7) break;
        sum += i;
    }
    printf("%s: break+continue\n", sum == (1 + 3 + 5) ? "PASS" : "FAIL");

    /* goto forward */
    i = 0;
    goto forward;
    i = 999;
forward:
    printf("%s: goto forward\n", i == 0 ? "PASS" : "FAIL");

    /* goto backward (loop via goto) */
    sum = 0;
    i = 0;
back:
    if (i < 5) {
        sum += i;
        i++;
        goto back;
    }
    printf("%s: goto backward\n", sum == 10 ? "PASS" : "FAIL");

    /* Deeply nested if/else */
    i = 42;
    int result;
    if (i > 100) {
        result = 1;
    } else if (i > 50) {
        result = 2;
    } else if (i > 40) {
        if (i > 45) {
            result = 3;
        } else if (i > 41) {
            if (i == 42) {
                result = 4;
            } else {
                result = 5;
            }
        } else {
            result = 6;
        }
    } else {
        result = 7;
    }
    printf("%s: deep if/else\n", result == 4 ? "PASS" : "FAIL");

    /* Loop with multiple exit paths */
    result = -1;
    for (i = 0; i < 100; i++) {
        if (i == 7 && i % 7 == 0) {
            result = 1;
            break;
        }
        if (i > 50) {
            result = 2;
            break;
        }
    }
    printf("%s: multi-exit loop\n", result == 1 ? "PASS" : "FAIL");

    /* Switch inside a loop */
    sum = 0;
    for (i = 0; i < 6; i++) {
        switch (i) {
            case 0: sum += 10; break;
            case 1: sum += 20; break;
            case 2: sum += 30; break;
            case 3: /* fallthrough */
            case 4: sum += 5; break;
            default: sum += 1; break;
        }
    }
    printf("%s: switch in loop\n", sum == (10 + 20 + 30 + 5 + 5 + 1) ? "PASS" : "FAIL");

    /* Short-circuit && — right side not evaluated if left is false */
    counter = 0;
    if (inc_false() && inc_true()) {
        /* should not reach */
    }
    printf("%s: short-circuit &&\n", counter == 1 ? "PASS" : "FAIL");

    /* Short-circuit || — right side not evaluated if left is true */
    counter = 0;
    if (inc_true() || inc_false()) {
        /* should reach via left side */
    }
    printf("%s: short-circuit ||\n", counter == 1 ? "PASS" : "FAIL");

    /* Combined short-circuit with side effects */
    counter = 0;
    int x = (inc_false() && inc_true()) || (inc_true() && inc_true());
    printf("%s: short-circuit combo\n", x == 1 && counter == 3 ? "PASS" : "FAIL");

    /* While loop with break */
    sum = 0;
    i = 0;
    while (i < 100) {
        sum += i;
        i++;
        if (sum > 10) break;
    }
    printf("%s: while break\n", sum == 15 ? "PASS" : "FAIL");

    /* Do-while */
    sum = 0;
    i = 0;
    do {
        sum += i;
        i++;
    } while (i < 5);
    printf("%s: do-while\n", sum == 10 ? "PASS" : "FAIL");

    return 0;
}
