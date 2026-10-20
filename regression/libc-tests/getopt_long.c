/* HOSTLEG -- getopt and getopt_long: clusters, attached and separate
 * arguments, "--", a lone "-", unknown options, missing arguments with
 * and without a leading ':', unique prefixes, flags, optional arguments,
 * and the operands getopt_long moves to the end of argv.  Only what the
 * GNU, musl and BSD libraries agree on: plain getopt sees its options
 * before its operands (glibc's permutes, the others stop), opterr is 0
 * (the messages differ), each scan starts over with optind = 1, and
 * optarg is looked at only after an option that takes an argument. */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>

static int quiet_flag;

static const struct option shelf_opts[] = {
    {"verbose", no_argument, 0, 'v'},
    {"version", no_argument, 0, 'V'},
    {"label", required_argument, 0, 'l'},
    {"output", required_argument, 0, 'o'},
    {"out", no_argument, 0, 'O'},
    {"colour", optional_argument, 0, 'k'},
    {"quiet", no_argument, &quiet_flag, 7},
    {0, 0, 0, 0}
};

static void show_rest(int argc, char **argv)
{
    int i;
    printf("  optind=%d argv:", optind);
    for (i = 1; i < argc; i++)
        printf(" %s", argv[i]);
    printf("\n");
}

static void show(int c, int idx)
{
    if (c == 0)
        printf("  flag (index %d) quiet=%d\n", idx, quiet_flag);
    else if (c == 1)
        printf("  operand %s\n", optarg);
    else if (c == '?' || c == ':')
        printf("  '%c' optopt=%d\n", c, optopt);
    else if (strchr("bclok", c))    /* what optarg holds after the */
        printf("  -%c %s\n", c, optarg ? optarg : "(none)");
    else                            /* others is unspecified */
        printf("  -%c\n", c);
}

/* After a missing argument POSIX puts optind past argc, and glibc
 * leaves it at argc: those scans (missing) do not show it. */
static void short_scan(const char *title, int argc, char **argv,
                       const char *spec, int missing)
{
    int c;
    printf("%s\n", title);
    optind = 1;
    while ((c = getopt(argc, argv, spec)) != -1)
        show(c, -1);
    if (!missing)
        show_rest(argc, argv);
}

static void long_scan(const char *title, int argc, char **argv,
                      const char *spec)
{
    int c, idx;
    printf("%s\n", title);
    optind = 1;
    quiet_flag = 0;
    for (;;) {
        idx = -1;
        c = getopt_long(argc, argv, spec, shelf_opts, &idx);
        if (c == -1)
            break;
        if (c == 0)
            show(c, idx);
        else
            show(c, -1);
    }
    show_rest(argc, argv);
}

int main(void)
{
    opterr = 0;
    {
        char *v[] = {"jars", "-a", "-b", "plum", "-cdamson", "-ab", "fig",
                     "--", "-a", "tail", 0};
        short_scan("getopt: separate, attached, clustered, --", 10, v, "ab:c:",
                   0);
    }
    {
        char *v[] = {"jars", "-a", "-", "-b", "x", 0};
        short_scan("getopt: a lone - is an operand", 5, v, "ab:", 0);
    }
    {
        char *v[] = {"jars", "-z", "-a", "-b", 0};
        short_scan("getopt: unknown, then missing argument", 4, v, "ab:", 1);
    }
    {
        char *v[] = {"jars", "-b", 0};
        short_scan("getopt: missing argument, leading colon", 2, v, ":ab:", 1);
    }
    {
        char *v[] = {"jars", "pears.txt", "--label=WINTER", "quince.txt", "-v",
                     "--output", "shelf.aws", "medlar.txt", 0};
        long_scan("getopt_long: operands moved after the options", 8, v,
                  "vVl:o:");
    }
    {
        char *v[] = {"jars", "--lab", "SPRING", "--outp=crate", "--out",
                     "--quiet", "--colour", "--colour=amber", 0};
        long_scan("getopt_long: prefixes, exact match, flag, optional", 8, v,
                  "vVl:o:");
    }
    {
        char *v[] = {"jars", "--ver", "--verbose=yes", "--nothing", "--label",
                     0};
        long_scan("getopt_long: ambiguous, needless argument, unknown, missing",
                  5, v, "vVl:o:");
    }
    {
        /* (musl gets this one wrong: it rotates argv's closing null in) */
        char *v[] = {"jars", "plum.txt", "-o", 0};
        long_scan("getopt_long: an operand, then a missing argument", 3, v,
                  "vVl:o:");
    }
    {
        char *v[] = {"jars", "-v", "--label", 0};
        long_scan("getopt_long: missing argument, leading colon", 3, v,
                  ":vVl:o:");
    }
    {
        char *v[] = {"jars", "-v", "apricot", "-V", 0};
        long_scan("getopt_long: + stops at the first operand", 4, v, "+vV");
    }
    {
        char *v[] = {"jars", "-v", "apricot", "-V", "cherry", 0};
        long_scan("getopt_long: - returns operands in place", 5, v, "-vV");
    }
    {
        char *v[] = {"jars", "-vlDAMP", "elder", "-o", "x", "--", "-V", 0};
        long_scan("getopt_long: cluster with argument, -- after operands", 7, v,
                  "vVl:o:");
    }
    return 0;
}
