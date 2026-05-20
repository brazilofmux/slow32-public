/* Tests for-loop, if, comparison, accumulation. */

int main(void) {
    int sum;
    int i;
    sum = 0;
    for (i = 1; i <= 10; i = i + 1) {
        if (i == 7) sum = sum - 1;
        else        sum = sum + i;
    }
    /* 1+2+3+4+5+6 + (-1) + 8+9+10 = 21 - 1 + 27 = 47 */
    return sum;
}
