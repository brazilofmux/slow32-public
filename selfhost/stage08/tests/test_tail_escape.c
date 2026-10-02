/* selfhost ISSUES-76: a tail call must not hand its callee a pointer
 * into the frame it has just popped.
 *
 * `return check(buf, depth)` with `buf` a local array was compiled as:
 * restore the saved registers, pop the frame, jump to check.  The array
 * is then below the stack pointer, and check and whatever it calls
 * build their frames over it.  A shallow callee with a small frame may
 * not reach the bytes that matter and reads them intact; a deep one
 * overwrites them.  That was s32-as's spurious symbol (handle ->
 * add_reloc_ex -> get_lbl -> a table's growth), unexplained for a
 * month.
 *
 * Here the arrays are large and the callees write over frames as large
 * before they look, so a popped frame is certainly written on.
 *
 * Returns 0, or the number of the first check that fails. */

int deep(int n) {
    char pad[96];
    int i;
    i = 0;
    while (i < 96) {
        pad[i] = 0x5a;
        i = i + 1;
    }
    if (n > 0) return deep(n - 1) + pad[n & 63];
    return pad[0];
}

/* every byte of a 240-byte array holds its index */
int check(char *buf, int depth) {
    char scratch[240];
    int i;
    i = 0;
    while (i < 240) {
        scratch[i] = 0x44;
        i = i + 1;
    }
    deep(depth + scratch[7] - 0x44);
    i = 0;
    while (i < 240) {
        if (buf[i] != (i & 63)) return 0;
        i = i + 1;
    }
    return 1;
}

int sum(int *v, int depth) {
    int scratch[60];
    int i;
    int t;
    i = 0;
    while (i < 60) {
        scratch[i] = -1;
        i = i + 1;
    }
    deep(depth + scratch[7] + 1);
    t = 0;
    i = 0;
    while (i < 60) {
        t = t + v[i];
        i = i + 1;
    }
    return t;
}

/* the address of a local array, as an argument of the returned call */
int by_array(int depth) {
    char buf[240];
    int i;
    i = 0;
    while (i < 240) {
        buf[i] = i & 63;
        i = i + 1;
    }
    return check(buf, depth);
}

/* the same address by way of a pointer variable */
int by_pointer(int depth) {
    int v[60];
    int *p;
    int i;
    i = 0;
    while (i < 60) {
        v[i] = i;
        i = i + 1;
    }
    p = v;
    return sum(p, depth);
}

/* an address computed from a local's, not the local's itself */
int by_element(int depth) {
    char big[300];
    int i;
    i = 0;
    while (i < 240) {
        big[i + 40] = i & 63;
        i = i + 1;
    }
    return check(big + 40, depth);
}

/* no local's address goes anywhere: this one may be a tail call still */
int plain(int a, int depth) {
    int b;
    b = a + 1;
    if (b > 100) return 0;
    return deep(depth + b);
}

int main(void) {
    if (!by_array(0)) return 1;
    if (!by_array(1)) return 2;
    if (!by_array(8)) return 3;
    if (by_pointer(0) != 1770) return 4;
    if (by_pointer(8) != 1770) return 5;
    if (!by_element(0)) return 6;
    if (!by_element(8)) return 7;
    if (plain(4, 3) != 9 * 0x5a) return 8;      /* deep(8): nine frames' worth */
    return 0;
}
