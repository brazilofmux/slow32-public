/* Single fused compare-branch via `if`/`else`. Each comparison here is
   single-use feeding directly into a BRC, which is exactly the shape
   `hcg_identify_fusions` marks. */
int max3(int a, int b, int c) {
    int m;
    if (a > b) {
        m = a;
    } else {
        m = b;
    }
    if (c > m) {
        m = c;
    }
    return m;
}

int main(void) {
    return max3(3, 7, 5) - 7;
}
