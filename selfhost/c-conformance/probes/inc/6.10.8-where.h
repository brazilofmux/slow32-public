/* helper for 6.10.8 probes: reports its own presumed name and line */
static const char *where_file(void) { return __FILE__; }
static int where_line(void) { return __LINE__; }
