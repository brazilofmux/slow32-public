/* Test pointer arithmetic scaling.
 * int* should advance by 4 bytes per element.
 * char* should advance by 1 byte per element. */
int main(void) {
    int arr[4];
    int *p;
    int diff;
    char *cp;
    char cbuf[4];

    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 40;

    /* ptr + int (scale by 4) */
    p = &arr[0];
    p = p + 2;
    if (*p != 30) return 1;

    /* ptr - int (scale by 4) */
    p = p - 1;
    if (*p != 20) return 2;

    /* ptr - ptr (element count) */
    diff = p - &arr[0];
    if (diff != 1) return 3;

    /* ++ptr (scale by 4) */
    ++p;
    if (*p != 30) return 4;

    /* char* + int (scale by 1) */
    cbuf[0] = 'A';
    cbuf[1] = 'B';
    cbuf[2] = 'C';
    cp = &cbuf[0];
    cp = cp + 2;
    if (*cp != 'C') return 5;

    /* ptr += n (scale by 4) */
    p = &arr[0];
    p += 3;
    if (*p != 40) return 6;

    return 0;
}
