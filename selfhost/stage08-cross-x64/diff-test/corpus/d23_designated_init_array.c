/* Designated initializers for arrays -- sparse indices, sequential
 * continuations after a designator, later writes overriding earlier
 * ones, and inferred array size from the maximum reached index. */

static int sparse[10] = { [0] = 1, [3] = 4, [5] = 25, [9] = -1 };
static int chain[8]   = { [2] = 200, 300, 400, [1] = 100 };
static int autoarr[]  = { [4] = 50, 60, [0] = 10, 20 };

static int sum_with_weights(int *a, int n) {
    int s = 0;
    int i;
    for (i = 0; i < n; i++) {
        s = s + a[i] * (i + 1);
    }
    return s;
}

int main(void) {
    int local[6] = { [5] = 600, [0] = 10, 20, [3] = 40, 50 };
    int autoarr_n = (int)(sizeof(autoarr) / sizeof(autoarr[0]));
    int acc = 0;
    acc = acc * 17 + sum_with_weights(sparse, 10);
    acc = acc * 17 + sum_with_weights(chain, 8);
    acc = acc * 17 + sum_with_weights(autoarr, autoarr_n);
    acc = acc * 17 + sum_with_weights(local, 6);
    acc = acc * 17 + autoarr_n;
    return acc & 0xff;
}
