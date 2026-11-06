#ifdef __cplusplus
extern "C" {
#endif
int setenv(const char *, const char *, int); int unsetenv(const char *);
unsigned getuid(void); unsigned getgid(void);
int gethostname(char *, unsigned); int mkstemp(char *);
#ifdef __cplusplus
}
#endif
#define _SC_CLK_TCK 2
