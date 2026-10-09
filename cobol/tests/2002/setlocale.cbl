*> SET LOCALE (2023 14.9.39 formats 11 and 12; docs/plans/locale.md step 1):
*> a category or LC_ALL, USER-DEFAULT (the environment's: this test's .env
*> sets LANG=sv_SE.UTF-8), SYSTEM-DEFAULT (POSIX), the locale saved into a
*> data-pointer and taken back, the user default changed, a called program's
*> switch persisting in its caller (14.6.6 rule 9), a pointer that is not a
*> saved locale: EC-LOCALE-INVALID-PTR, fatal, so the run unit ends after
*> the declarative. GnuCOBOL spells the locale as a string: no oracle;
*> ICU's orders are the witness.
identification division.
program-id. setlocale.
environment division.
configuration section.
special-names.
    locale german is "de_DE.UTF-8"
    locale czech is "cs_CZ".
data division.
working-storage section.
01 saved usage pointer.
01 saved2 usage pointer.
01 junk pic x(8) value "notsaved".
01 junkptr usage pointer.
procedure division.
declaratives.
d section.
    use after exception condition ec-locale-invalid-ptr.
d1.
    display "  EC-LOCALE-INVALID-PTR".
end declaratives.
main section.
m1.
    display "user default (LANG sv) z/å " function locale-compare("z" "å")
    set locale lc_all to system-default
    display "system default z/å " function locale-compare("z" "å")
    set locale lc_collate to german
    display "german ä/b " function locale-compare("ä" "b")
    set saved to locale lc_all
    set locale lc_all to czech
    display "czech ch/h " function locale-compare("ch" "h")
    set locale lc_all to saved
    display "restored ch/h " function locale-compare("ch" "h") " ä/b " function locale-compare("ä" "b")
    set locale lc_all to user-default
    display "user default again z/å " function locale-compare("z" "å")
    set locale user-default to german
    set locale lc_collate to user-default
    display "user default now german z/å " function locale-compare("z" "å")
    set saved2 to locale user-default
    call "setsub"
    display "after the call ch/h " function locale-compare("ch" "h")
    set locale lc_collate to saved2
    display "from the saved user default ch/h " function locale-compare("ch" "h")
    set locale lc_time to czech
    display "lc_time switched, lc_collate not ch/h " function locale-compare("ch" "h")
    set junkptr to address of junk
    >>turn ec-locale-invalid-ptr checking on
    set locale lc_all to junkptr
    display "not reached: EC-LOCALE-INVALID-PTR is fatal, the run unit ends after the declarative (14.6.12)"
    stop run.
end program setlocale.
identification division.
program-id. setsub.
environment division.
configuration section.
special-names.
    locale czech2 is "cs".
procedure division.
    set locale lc_all to czech2
    exit program.
end program setsub.
