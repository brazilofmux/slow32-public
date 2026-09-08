/* strtod / atof over the runtime's dtoa, which this libc already carries
 * for printf %f.  A file of its own, not posix_more.c: the gen1 tools are
 * linked against the libc *without* dtoa, and a reference from posix_more
 * would leave dtoa_strtod undefined there.  SQLite's shell registers
 * strtod() as an SQL function. */
extern double dtoa_strtod(const char *s00, char **se);
double strtod(const char *s, char **end) { return dtoa_strtod(s, end); }
double atof(const char *s) { return dtoa_strtod(s, (char **)0); }
