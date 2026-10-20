/* getopt, getopt_long, getopt_long_only: command-line options.
 *
 * One source for both C libraries (the clang runtime, and the
 * self-hosted library through selfhost/stage08/build-s12cc.sh), as
 * strerror.c is.  tapemgr asked for it: getopt_long, with its options
 * after the file names.
 *
 * The behaviour is the GNU interface as musl gives it: getopt() is
 * POSIX's and stops at the first operand; getopt_long() moves the
 * operands it passes over to the end of argv, so options may follow
 * them, unless the option string starts with '+' (stop at the first
 * operand) or '-' (return each operand as option 1, in optarg).
 * POSIXLY_CORRECT in the environment means '+'.  A ':' at the start (after
 * any '+' or '-') silences the messages and returns ':' for a missing
 * argument.  Setting optind to 0 starts over.
 *
 * "--" ends the options; a lone "-" is an operand.  A long option may
 * be abbreviated to any unique prefix; an exact match wins over a
 * longer name that it is a prefix of.
 */
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *optarg;
int optind = 1;
int opterr = 1;
int optopt = '?';

static char *nextchar;          /* the rest of a cluster of short options */
static int first_nonopt;        /* operands passed over: [first, last) */
static int last_nonopt;
static int started;

/* Move the operands passed over, argv[first_nonopt, last_nonopt), after
 * the options found since, argv[last_nonopt, optind), keeping each
 * group's order.  Three reversals do it in place. */
static void reverse(char **v, int lo, int hi)
{
    while (lo < --hi) {
        char *t = v[lo];
        v[lo++] = v[hi];
        v[hi] = t;
    }
}

static void exchange(char **v)
{
    reverse(v, first_nonopt, last_nonopt);
    reverse(v, last_nonopt, optind);
    reverse(v, first_nonopt, optind);
    first_nonopt += optind - last_nonopt;
    last_nonopt = optind;
}

static int is_operand(char *a)
{
    return a[0] != '-' || a[1] == '\0';
}

static void complain(const char *prog, const char *what, const char *dash,
                     const char *name)
{
    fprintf(stderr, "%s: option '%s%s' %s\n", prog, dash, name, what);
}

/* One long option at argv[optind] + skip.  Returns what getopt_long does;
 * -2 means it is not a long option after all (getopt_long_only: "-xy"
 * names no long option, and x is a short one, so read it as those). */
static int long_option(int argc, char **argv, const char *optstring,
                       const struct option *longopts, int *longindex,
                       int skip, int silent, int colon, int short_fallback)
{
    char *arg = argv[optind];
    char *name = arg + skip;
    const char *dash = skip == 2 ? "--" : "-";
    int namelen = 0;
    int found = -1, ambiguous = 0, exact = 0;
    int i;

    while (name[namelen] && name[namelen] != '=')
        namelen++;
    for (i = 0; longopts[i].name; i++) {
        if (strncmp(longopts[i].name, name, namelen) != 0)
            continue;
        if ((int)strlen(longopts[i].name) == namelen) {
            found = i;
            exact = 1;
            break;
        }
        if (found < 0)
            found = i;
        else if (longopts[found].has_arg != longopts[i].has_arg ||
                 longopts[found].flag != longopts[i].flag ||
                 longopts[found].val != longopts[i].val)
            ambiguous = 1;
    }
    if (!exact && ambiguous) {
        if (!silent)
            fprintf(stderr, "%s: option '%s%.*s' is ambiguous\n", argv[0], dash,
                    namelen, name);
        optind++;
        optopt = 0;
        return '?';
    }
    if (found < 0) {
        if (short_fallback && strchr(optstring, name[0]))
            return -2;
        if (!silent)
            fprintf(stderr, "%s: unrecognized option '%s%s'\n", argv[0], dash,
                    name);
        optind++;
        optopt = 0;
        return '?';
    }

    optind++;
    optarg = NULL;
    if (name[namelen] == '=') {
        if (longopts[found].has_arg == no_argument) {
            if (!silent)
                complain(argv[0], "doesn't allow an argument", dash,
                         longopts[found].name);
            optopt = longopts[found].flag ? 0 : longopts[found].val;
            return '?';
        }
        optarg = name + namelen + 1;
    } else if (longopts[found].has_arg == required_argument) {
        if (optind >= argc) {
            if (!silent)
                complain(argv[0], "requires an argument", dash,
                         longopts[found].name);
            optopt = longopts[found].flag ? 0 : longopts[found].val;
            return colon ? ':' : '?';
        }
        optarg = argv[optind++];
    }
    if (longindex)
        *longindex = found;
    if (longopts[found].flag) {
        *longopts[found].flag = longopts[found].val;
        return 0;
    }
    return longopts[found].val;
}

static int scan(int argc, char *const *argv_in, const char *optstring,
                const struct option *longopts, int *longindex, int long_only,
                int permute_ok)
{
    char **argv = (char **)argv_in;
    enum { PERMUTE, REQUIRE_ORDER, RETURN_IN_ORDER } ordering;
    int silent, colon;
    int c;
    const char *spec;

    ordering = permute_ok ? PERMUTE : REQUIRE_ORDER;
    if (*optstring == '-') {
        ordering = RETURN_IN_ORDER;
        optstring++;
    } else if (*optstring == '+') {
        ordering = REQUIRE_ORDER;
        optstring++;
    } else if (getenv("POSIXLY_CORRECT")) {
        ordering = REQUIRE_ORDER;
    }
    colon = *optstring == ':';
    silent = colon || !opterr;

    if (optind == 0 || !started) {
        if (optind == 0)
            optind = 1;
        first_nonopt = last_nonopt = optind;
        nextchar = NULL;
        started = 1;
    }
    optarg = NULL;

    if (nextchar == NULL || *nextchar == '\0') {
        if (optind > argc)          /* after a missing argument */
            optind = argc;
        if (last_nonopt > optind)
            last_nonopt = optind;
        if (first_nonopt > optind)
            first_nonopt = optind;
        if (ordering == PERMUTE) {
            if (first_nonopt != last_nonopt && last_nonopt != optind)
                exchange(argv);
            else if (last_nonopt != optind)
                first_nonopt = optind;
            while (optind < argc && is_operand(argv[optind]))
                optind++;
            last_nonopt = optind;
        }
        if (optind < argc && strcmp(argv[optind], "--") == 0) {
            optind++;
            if (first_nonopt != last_nonopt && last_nonopt != optind)
                exchange(argv);
            else if (first_nonopt == last_nonopt)
                first_nonopt = optind;
            last_nonopt = argc;
            optind = argc;
        }
        if (optind >= argc) {
            if (first_nonopt != last_nonopt)
                optind = first_nonopt;
            return -1;
        }
        if (is_operand(argv[optind])) {
            if (ordering == REQUIRE_ORDER)
                return -1;
            optarg = argv[optind++];
            return 1;
        }
        if (longopts && argv[optind][1] == '-')
            return long_option(argc, argv, optstring, longopts, longindex, 2,
                               silent, colon, 0);
        /* getopt_long_only: "-x" is a long option too, unless it is one
         * letter that is a short option */
        if (longopts && long_only &&
            (argv[optind][2] || !strchr(optstring, argv[optind][1]))) {
            c = long_option(argc, argv, optstring, longopts, longindex, 1,
                            silent, colon, 1);
            if (c != -2)
                return c;
        }
        nextchar = argv[optind] + 1;
    }

    c = (unsigned char)*nextchar++;
    spec = c == ':' ? NULL : strchr(optstring, c);
    if (*nextchar == '\0')
        optind++;
    if (spec == NULL) {
        if (!silent)
            fprintf(stderr, "%s: invalid option -- '%c'\n", argv[0], c);
        optopt = c;
        return '?';
    }
    if (spec[1] == ':') {
        if (spec[2] == ':') {
            /* an optional argument is only ever the rest of this word */
            if (*nextchar != '\0') {
                optarg = nextchar;
                optind++;
            }
        } else if (*nextchar != '\0') {
            optarg = nextchar;
            optind++;
        } else if (optind >= argc) {
            /* POSIX: past argc says the argument is missing (glibc
             * stops at argc; musl and the BSDs do this) */
            optind++;
            if (!silent)
                fprintf(stderr, "%s: option requires an argument -- '%c'\n",
                        argv[0], c);
            optopt = c;
            c = colon ? ':' : '?';
        } else {
            optarg = argv[optind++];
        }
        nextchar = NULL;
    }
    return c;
}

int getopt(int argc, char *const argv[], const char *optstring)
{
    return scan(argc, argv, optstring, NULL, NULL, 0, 0);
}

int getopt_long(int argc, char *const argv[], const char *optstring,
                const struct option *longopts, int *longindex)
{
    return scan(argc, argv, optstring, longopts, longindex, 0, 1);
}

int getopt_long_only(int argc, char *const argv[], const char *optstring,
                     const struct option *longopts, int *longindex)
{
    return scan(argc, argv, optstring, longopts, longindex, 1, 1);
}
