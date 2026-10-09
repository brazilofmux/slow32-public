*> LOCALE-COMPARE (2023 15.51) and the LOCALE clause (12.3.7; docs/plans/
*> locale.md step 1): collation by libutf's CLDR 46 tailorings, the
*> trimming rule of 8.8.4.2.11, national operands, the current locale.
*> GnuCOBOL spells the locale as a string and its oracle image has no
*> locales installed, so no oracle: ICU's orders (libutf's collate_locales
*> table, ICU 72 / CLDR 42) are the witness for every line.
identification division.
program-id. localecmp.
environment division.
configuration section.
special-names.
    locale swedish is "sv_SE.UTF-8"
    locale german is de-DE
    locale czech is "cs"
    locale danish is "da_DK"
    locale turkish is "tr_TR.UTF-8"
    locale canadian is "fr-CA"
    locale posix is "POSIX".
data division.
working-storage section.
01 r pic x.
01 a pic x(10) value "ab".
01 b pic x(10) value "ab   ".
01 nz pic n(1) value n"z".
01 blanks pic x(5) value spaces.
01 one pic x value space.
procedure division.
    display "sv z/å " function locale-compare("z" "å" swedish)
        " å/ä " function locale-compare("å" "ä" swedish)
        " ä/ö " function locale-compare("ä" "ö" swedish)
        " äb/ab " function locale-compare("äb" "ab" swedish)
    display "de z/å " function locale-compare("z" "å" german)
        " ä/b " function locale-compare("ä" "b" german)
    display "cs ch/h " function locale-compare("ch" "h" czech)
        " ch/i " function locale-compare("ch" "i" czech)
        " posix ch/h " function locale-compare("ch" "h" posix)
    display "da A/a " function locale-compare("A" "a" danish)
        " aa/z " function locale-compare("aa" "z" danish)
        " posix A/a " function locale-compare("A" "a" posix)
    display "tr ı/i " function locale-compare("ı" "i" turkish)
        " ı/h " function locale-compare("ı" "h" turkish)
    display "fr-CA côte/coté " function locale-compare("côte" "coté" canadian)
        " posix " function locale-compare("côte" "coté" posix)
    display "trim " function locale-compare(a b posix)
        " " function locale-compare(blanks one posix)
        " " function locale-compare(blanks "x" posix)
    display "national " function locale-compare(nz "å" swedish)
        " " function locale-compare(nz "z" posix)
    set locale lc_all to swedish
    display "current sv " function locale-compare("z" "å")
    set locale lc_collate to posix
    display "current posix " function locale-compare("z" "å")
    display "equal " function locale-compare("abc" "abc" german)
    move function locale-compare("b" "a" german) to r
    display "r=" r
    stop run.
