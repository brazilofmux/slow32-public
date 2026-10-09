*> STANDARD-COMPARE (2023 15.85) and ORDER TABLE (12.3.7 rule 17; docs/plans/
*> locale.md step 1): the DUCET of Unicode 16.0 under the UCA, answering to
*> 'ISO_14651_2020_TABLE1'; argument-4 the level, the sort keys cut there;
*> the trimming rule; a level the table has not is EC-ORDER-NOT-SUPPORTED,
*> the full comparison returned. GnuCOBOL has no STANDARD-COMPARE, so
*> no oracle; ICU's root collation is the witness.
identification division.
program-id. stdcompare.
environment division.
configuration section.
special-names.
    order table ducet is "ISO_14651_2020_TABLE1".
data division.
working-storage section.
01 lv pic 9 value 1.
01 r pic x.
procedure division.
declaratives.
d section.
    use after exception condition ec-order-not-supported.
d1.
    display "  EC-ORDER-NOT-SUPPORTED".
end declaratives.
main section.
m1.
    display "a/b " function standard-compare("a" "b")
        " b/a " function standard-compare("b" "a")
        " abc/abc " function standard-compare("abc" "abc")
    display "a/A " function standard-compare("a" "A")
        " L1 " function standard-compare("a" "A" 1)
        " L2 " function standard-compare("a" "A" 2)
        " L3 " function standard-compare("a" "A" 3)
        " L4 " function standard-compare("a" "A" 4)
    display "a/á L1 " function standard-compare("a" "á" 1)
        " L2 " function standard-compare("a" "á" 2)
    display "resume/résumé L1 " function standard-compare("resume" "résumé" ducet 1)
        " full " function standard-compare("resume" "résumé" ducet)
    display "Å/A+ring L4 " function standard-compare("Å" "Å" 4)
        " ch/h " function standard-compare("ch" "h")
        " ä/b " function standard-compare("ä" "b")
    display "trim " function standard-compare("ab   " "ab")
        " " function standard-compare("     " " ")
        " " function standard-compare("ab" "abc" 1)
    display "level in an item " function standard-compare("a" "A" lv)
    move 4 to lv
    display "level 4 in an item " function standard-compare("a" "A" lv)
    move 5 to lv
    display "level 5, unchecked " function standard-compare("a" "A" lv)
    >>turn ec-order-not-supported checking on
    move function standard-compare("a" "A" lv) to r
    display "not reached: EC-ORDER-NOT-SUPPORTED is fatal, the run unit ends after the declarative (14.6.12)"
    stop run.
