*> The command line, GnuCOBOL's implementor module: ARGUMENT-NUMBER is
*> the count, ARGUMENT-VALUE the arguments in turn, DISPLAY n UPON
*> ARGUMENT-NUMBER repositions, past the end leaves the item alone,
*> COMMAND-LINE is the whole thing.  args.args supplies "202608 second".
*> An implementor module: default dialect.
identification division.
program-id. args.
data division.
working-storage section.
01  n    pic 99.
01  v    pic x(10).
01  cl   pic x(30).
01  ws-arg-index pic 9 comp value 1.
01  ym.
    05  yy pic 9999.
    05  mm pic 99.
procedure division.
    accept n from argument-number.
    display 'count=' n.
    move all '*' to v.
    accept v from argument-value.
    display 'first=[' v ']'.
    accept v from argument-value.
    display 'second=[' v ']'.
    display 1 upon argument-number.
    accept v from argument-value.
    display 'again first=[' v ']'.
    move all '*' to v.
    accept v from argument-value.
    accept v from argument-value.
    display 'past end=[' v ']'.
    accept cl from command-line.
    display 'cl=[' cl ']'.
    display ws-arg-index upon argument-number.
    accept ym from argument-value.
    display 'year ' yy ' month ' mm.
    stop run.
