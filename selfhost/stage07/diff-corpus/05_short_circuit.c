/* Short-circuit chain — the exact shape that the issue traces back to:
   `n->lhs && hl_struct_ret && ty_is_struct(...)` in hl_stmt. Each
   condition is a fusion candidate, and the LHS pointer is needed
   across multiple BRCs.

   No struct-arrow yet — see 08_struct_arrow.c for the full pattern. */
int g;

int probe(int x) {
    g = g + 1;
    return x;
}

int and3(int a, int b, int c) {
    if (probe(a) && probe(b) && probe(c)) {
        return 1;
    }
    return 0;
}

int or3(int a, int b, int c) {
    if (probe(a) || probe(b) || probe(c)) {
        return 1;
    }
    return 0;
}

int main(void) {
    g = 0;
    if (and3(1, 2, 3) != 1) return 1;
    if (or3(0, 0, 5) != 1) return 2;
    if (or3(0, 0, 0) != 0) return 3;
    return 0;
}
