/* Six-parameter function with each parameter consumed by a fused
   compare-branch followed by reuse. Forces spill/reload activity
   across multiple fusion sites — the precise interaction
   `ra_extend_fused_cmp` is meant to handle. */
int score(int a, int b, int c, int d, int e, int f) {
    int s;
    s = 0;
    if (a < b) s = s + a;
    if (b < c) s = s + b;
    if (c < d) s = s + c;
    if (d < e) s = s + d;
    if (e < f) s = s + e;
    /* Re-use of every parameter after the chain forces the allocator
       to keep them live across all the fusion points. */
    return s + a + b + c + d + e + f;
}

int main(void) {
    return score(1, 2, 3, 4, 5, 6) - (1+2+3+4+5 + 1+2+3+4+5+6);
}
