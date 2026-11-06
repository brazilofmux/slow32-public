#include <stdio.h>
#include <complex.h>
extern int addup_(const int *n);
extern float _Complex fcf(float _Complex a, int n);
extern double _Complex fcd(double _Complex a, double _Complex b);
extern void fstr(char *s, int n);
extern int arrsum_(const int *a, const int *n);
extern double rmul_(const double *x, const double *y);
int main(void) {
  int n = 100, a[4] = {1,2,3,4}, m = 4; char s[] = "hello"; double x = 3, y = 4;
  printf("addup %d\n", addup_(&n));
  float _Complex f = fcf(1.5f + 2.0f * I, 4); printf("fcf %.2f %.2f\n", crealf(f), cimagf(f));
  double _Complex d = fcd(1.0 + 2.0 * I, 3.0 + 4.0 * I); printf("fcd %.2f %.2f\n", creal(d), cimag(d));
  fstr(s, 5); printf("fstr %s\n", s);
  printf("arrsum %d\n", arrsum_(a, &m));
  printf("rmul %.2f\n", rmul_(&x, &y));
  return 0;
}
