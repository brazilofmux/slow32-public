identification division.
program-id. natgroup.
*> National groups (2023 13.18.29; cobol ISSUES-71): GROUP-USAGE
*> NATIONAL makes a group one national item, PICTURE N(m) for its m
*> characters -- a MOVE into it converts and pads with national spaces,
*> as a whole; DISPLAY, comparison, LENGTH, reference modification and
*> INSPECT see it as one national item.  A subordinate group is a
*> national group too (rule 3).  INITIALIZE and MOVE CORRESPONDING
*> process it as a group (14.9.20.4 rule 1, 14.9.26 note 5), and a
*> group that is not national receives its bytes (14.9.25 rule 4).
*> No oracle (docs/national.md).
data division.
working-storage section.
01  ng group-usage national.
    05 a     pic n(3).
    05 b.
       10 c  pic n(2).
01  ng2 group-usage is national value n"東京都港区".
    05 a     pic n(3).
    05 c     pic n(2).
01  plain.
    05 x     pic x(10).
01  k        pic 99.
procedure division.
main.
    move n"日本" to ng
    display "moved: [" ng "] [" a of ng "] [" c of ng "]"
    move "abcdefg" to ng
    display "from alphanumeric: [" ng "]"
    if ng = n"abcde" display "compare: equal" end-if
    display "length: " function length(ng) " " function byte-length(ng)
    display "reference-modified: [" ng(2:3) "]"
    move 0 to k
    inspect ng tallying k for all n"c"
    display "inspect: " k
    display "value: [" ng2 "]"
    initialize ng
    display "initialized: [" ng "]"
    move corresponding ng2 to ng
    display "corresponding: [" ng "]"
    move ng to plain
    if x(1:2) = x"6771" display "a plain group receives the bytes" end-if
    stop run.
