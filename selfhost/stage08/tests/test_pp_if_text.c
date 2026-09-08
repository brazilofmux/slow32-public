/* GitHub issue 45: a text-bodied object-like macro in #if is a
 * full expression, not a primary overlaid onto the name. */
#define N 1000000000
#if N == 1000000000
int n_ok = 1;
#else
int n_ok = 0;
#endif
#define M 1+2
#if M * 3 == 9
int m_ok = 1;
#else
int m_ok = 0;
#endif
#define INNER 1+2
#define OUTER INNER * 3
#if OUTER == 9
int nest_ok = 1;
#else
int nest_ok = 0;
#endif

int main(void) {
    if (!n_ok) return 1;
    if (!m_ok) return 2;
    if (!nest_ok) return 3;
    return 0;
}
