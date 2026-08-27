/* The same LINPACK kernel as linpack.f, written to mirror it exactly:
 * identical loop structure, identical COLUMN-MAJOR indexing (a[(i-1) +
 * (j-1)*lda]), identical by-reference argument passing.  The point is
 * to compare CODE GENERATION for the same work, not two different
 * algorithms or two different memory access patterns. */

static void daxpy(int n, double da, double *dx, double *dy) {
    int i;
    if (n <= 0) return;
    if (da == 0.0) return;
    for (i = 1; i <= n; i++) dy[i-1] = dy[i-1] + da * dx[i-1];
}

static void dscal(int n, double da, double *dx) {
    int i;
    if (n <= 0) return;
    for (i = 1; i <= n; i++) dx[i-1] = da * dx[i-1];
}

static double dabs_(double v) { return v < 0.0 ? -v : v; }

static int idamax(int n, double *dx) {
    double dmax;
    int i, r;
    if (n < 1) return 0;
    if (n == 1) return 1;
    r = 1;
    dmax = dabs_(dx[0]);
    for (i = 2; i <= n; i++) {
        if (dabs_(dx[i-1]) <= dmax) continue;
        r = i;
        dmax = dabs_(dx[i-1]);
    }
    return r;
}

#define A(i,j) a[((i)-1) + ((j)-1)*lda]

static void dgefa(double *a, int lda, int n, int *ipvt, int *info) {
    double t;
    int j, k, kp1, l, nm1;
    *info = 0;
    nm1 = n - 1;
    if (nm1 >= 1) {
        for (k = 1; k <= nm1; k++) {
            kp1 = k + 1;
            l = idamax(n-k+1, &A(k,k)) + k - 1;
            ipvt[k-1] = l;
            if (A(l,k) == 0.0) { *info = k; continue; }
            if (l != k) { t = A(l,k); A(l,k) = A(k,k); A(k,k) = t; }
            t = -1.0 / A(k,k);
            dscal(n-k, t, &A(k+1,k));
            for (j = kp1; j <= n; j++) {
                t = A(l,j);
                if (l != k) { A(l,j) = A(k,j); A(k,j) = t; }
                daxpy(n-k, t, &A(k+1,k), &A(k+1,j));
            }
        }
    }
    ipvt[n-1] = n;
    if (A(n,n) == 0.0) *info = n;
}

static void dgesl(double *a, int lda, int n, int *ipvt, double *b) {
    double t;
    int k, kb, l, nm1;
    nm1 = n - 1;
    if (nm1 >= 1) {
        for (k = 1; k <= nm1; k++) {
            l = ipvt[k-1];
            t = b[l-1];
            if (l != k) { b[l-1] = b[k-1]; b[k-1] = t; }
            daxpy(n-k, t, &A(k+1,k), &b[k]);
        }
    }
    for (kb = 1; kb <= n; kb++) {
        k = n + 1 - kb;
        b[k-1] = b[k-1] / A(k,k);
        t = -b[k-1];
        daxpy(k-1, t, &A(1,k), &b[0]);
    }
}

static double a_[64*64];
static double aa_[64*64];
static double b_[64];
static int    ipvt_[64];

int main(void) {
    int lda = 64, n = 64, reps = 400;
    int i, j, r, info;
    double resid;
    double *a = aa_;

    for (j = 1; j <= n; j++) {
        for (i = 1; i <= n; i++) A(i,j) = 1.0 / (double)(i + j);
        A(j,j) = A(j,j) + (double)n;
    }
    for (r = 1; r <= reps; r++) {
        double *src = aa_;
        a = a_;
        for (j = 1; j <= n; j++)
            for (i = 1; i <= n; i++)
                a_[(i-1) + (j-1)*lda] = src[(i-1) + (j-1)*lda];
        for (i = 1; i <= n; i++) {
            b_[i-1] = 0.0;
            for (j = 1; j <= n; j++) b_[i-1] = b_[i-1] + src[(i-1) + (j-1)*lda];
        }
        dgefa(a_, lda, n, ipvt_, &info);
        if (info != 0) return 91;
        dgesl(a_, lda, n, ipvt_, b_);
    }
    resid = 0.0;
    for (i = 1; i <= n; i++) {
        double d = dabs_(b_[i-1] - 1.0);
        if (d > resid) resid = d;
    }
    if (resid > 1e-8) return 92;
    return 0;
}
