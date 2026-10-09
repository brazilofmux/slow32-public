*> EC-LOCALE-MISSING (2023 8.2, 14.9.39.4 rule 24; docs/plans/locale.md): the
*> environment names a locale this runtime has not (this test's .env sets
*> LANG=tlh_KL.UTF-8), so the user default is POSIX standing in for it; an
*> operation needing the current locale sets the condition and gets POSIX's
*> answer (fatal: the run unit ends after the declarative); a named locale
*> is fine. No oracle (GnuCOBOL's locale is a string); ICU's orders are the witness.
identification division.
program-id. localemiss.
environment division.
configuration section.
special-names.
    locale swedish is "sv".
data division.
working-storage section.
01 r pic x.
procedure division.
declaratives.
d section.
    use after exception condition ec-locale-missing.
d1.
    display "  EC-LOCALE-MISSING".
end declaratives.
main section.
m1.
    display "unchecked z/å " function locale-compare("z" "å")
    display "named sv z/å " function locale-compare("z" "å" swedish)
    set locale lc_all to swedish
    display "set sv z/å " function locale-compare("z" "å")
    set locale lc_all to user-default
    display "user default again z/å " function locale-compare("z" "å")
    >>turn ec-locale-missing checking on
    move function locale-compare("z" "å") to r
    display "not reached: EC-LOCALE-MISSING is fatal, the run unit ends after the declarative (14.6.12)"
    stop run.
