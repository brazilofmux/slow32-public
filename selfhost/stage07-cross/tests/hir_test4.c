/* Test pointers and arrays */

int main() {
    int arr[5];
    int *p;
    int i;
    int sum;

    /* Fill array */
    i = 0;
    while (i < 5) {
        arr[i] = (i + 1) * 10;
        i = i + 1;
    }

    /* Sum via pointer */
    sum = 0;
    p = arr;
    i = 0;
    while (i < 5) {
        sum = sum + *p;
        p = p + 1;
        i = i + 1;
    }

    /* 10+20+30+40+50 = 150 */
    return sum;
}
