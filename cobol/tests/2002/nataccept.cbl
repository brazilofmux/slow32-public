identification division.
program-id. nataccept.
*> ACCEPT into national items (cobol ISSUES-70): the text arrives as
*> UTF-8 and is moved -- a line from standard input, the command line,
*> an argument, the date -- truncated or padded with national spaces by
*> character.  The third line holds X"E9", Latin-1's e-acute, which
*> begins no UTF-8 character: U+FFFD, and once checking is on,
*> EC-DATA-CONVERSION (nonfatal).  At end of input the item is unchanged
*> and no condition is raised.
*> No oracle (docs/national.md).
data division.
working-storage section.
01  n        pic n(5).
01  w        pic n(8).
procedure division.
declaratives.
dc section.
    use after exception condition ec-data-conversion.
d1.
    display "  declarative: " function exception-status(1:18).
end declaratives.
main section.
m1.
    accept n
    display "long line: [" n "]"
    accept n
    display "short line: [" n "]"
    accept n
    display "invalid UTF-8, unchecked: [" n "]"
>>TURN EC-DATA-CONVERSION CHECKING ON
    accept n
    display "invalid UTF-8, checked: [" n "]"
    accept n
    display "at end: [" n "]"
    accept w from command-line
    display "command line: [" w "]"
    accept n from argument-value
    display "argument: [" n "]"
    accept w from date
    display "date: [" w "]"
    stop run.
