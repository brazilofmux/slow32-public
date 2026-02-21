typedef char *va_list;

int sum_var(int n, ...) {
    va_list ap;
    int i;
    int s;
    int v;
    s = 0;
    i = 0;
    va_start(ap, n);
    while (i < n) {
        v = va_arg(ap, int);
        s = s + v;
        i = i + 1;
    }
    va_end(ap);
    return s;
}

int main(void) {
    /* No varargs */
    if (sum_var(0) != 0) return 1;

    /* Small call: likely register varargs on most ABIs */
    if (sum_var(4, 1, 2, 3, 4) != 10) return 2;

    /* Cross register/stack boundary */
    if (sum_var(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) != 55) return 3;

    /* Sign behavior */
    if (sum_var(6, -1, -2, -3, 4, 5, 6) != 9) return 4;

    return 0;
}
