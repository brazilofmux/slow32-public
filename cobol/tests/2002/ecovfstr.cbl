*> EC-OVERFLOW-STRING, EC-OVERFLOW-UNSTRING and EC-RANGE-SEARCH-NO-MATCH
*> (2023 Table 13; 14.9.43.4 rule 8b, 14.9.48.4, 14.9.37.4): nonfatal
*> conditions, raised when checking is on, the statement's own phrase
*> (ON OVERFLOW, AT END) still taken after the declarative.  The last
*> STRING overflows with checking off: nothing is raised.
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
identification division.
program-id. ecovfstr.
data division.
working-storage section.
01  small pic x(3).
01  n pic 9 value 0.
01  wds pic x(7) value "a b c d".
01  t.
    05 e pic x occurs 3 times indexed by ix value "a".
procedure division.
declaratives.
d1 section.
    use after exception condition ec-overflow-string ec-overflow-unstring ec-range-search-no-match.
    display "declarative: " function exception-status.
end declaratives.
main section.
>>TURN EC-OVERFLOW EC-RANGE-SEARCH-NO-MATCH CHECKING ON
    string "abcdef" delimited by size into small
        on overflow display "on overflow: " small
    end-string
    unstring wds delimited by space into small small
        on overflow display "unstring overflow"
    end-unstring
    set ix to 1
    search e at end display "search: at end" when e(ix) = "z" display "found" end-search
>>TURN EC-OVERFLOW CHECKING OFF
    string "ghijkl" delimited by size into small
        on overflow display "overflow, checking off: " small
    end-string
    stop run.
