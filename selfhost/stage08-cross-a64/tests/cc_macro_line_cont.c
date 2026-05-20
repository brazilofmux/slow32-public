/* Multi-line #define with backslash-newline line continuation.
   Regression for shadow_interp.c-style macros (do { ... } while(0)). */

#define DBL(x) ((x) \
    + (x))

#define LOAD_PAIR(out, base, idx) do { \
    int v0_ = base[(idx) * 2]; \
    int v1_ = base[(idx) * 2 + 1]; \
    out = v0_ + v1_; \
} while (0)

int main(void) {
    int arr[4];
    int sum;
    arr[0] = 3;
    arr[1] = 4;
    arr[2] = 7;
    arr[3] = 8;
    LOAD_PAIR(sum, arr, 1);
    return DBL(sum) == 30;
}
