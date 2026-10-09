*> LOCALE-DATE, LOCALE-TIME and LOCALE-TIME-FROM-SECONDS (2023 15.52-15.54;
*> docs/plans/locale.md step 2): CLDR 46's medium date and time formats as
*> d_fmt and t_fmt, from the LOCALE clause's locale or the current LC_TIME;
*> the POSIX locale is the standard's own (%m/%d/%y, %H:%M:%S); a national
*> argument; an argument outside the rules is EC-ARGUMENT-FUNCTION (fatal:
*> the run unit ends after the declarative) and an empty result. No oracle: GnuCOBOL formats with the C library's locales,
*> absent from its image; the witness is ICU (78, CLDR 48) formatting the
*> same values -- every line here is what it printed, but for the spaces
*> CLDR 46 writes as U+202F (before AM/PM in en and el, before the year
*> word in uk), which CLDR 48 has as plain spaces: the data here is 46's.
identification division.
program-id. localedate.
environment division.
configuration section.
special-names.
    locale swedish is "sv_SE.UTF-8"
    locale german is "de_DE"
    locale us is "en_US"
    locale quebec is "fr_CA"
    locale turkish is "tr"
    locale czech is "cs_CZ"
    locale hungarian is "hu"
    locale greek is "el"
    locale ukrainian is "uk"
    locale posix is "POSIX".
data division.
working-storage section.
01 d8 pic x(8) value "20000229".
01 n8 pic n(8) value n"19990101".
01 t6 pic x(6) value "090507".
01 secs pic 9(5)v99 value 49512.75.
01 r pic x(30).
procedure division.
declaratives.
d section.
    use after exception condition ec-argument-function.
d1.
    display "  EC-ARGUMENT-FUNCTION".
end declaratives.
main section.
m1.
    display "sv [" function locale-date("20261008" swedish) "] [" function locale-time("134512" swedish) "]"
    display "de [" function locale-date("20261008" german) "] [" function locale-time("134512" german) "]"
    display "en_US [" function locale-date("20261008" us) "] [" function locale-time("134512" us) "]"
    display "fr_CA [" function locale-date("20261008" quebec) "] [" function locale-time("134512" quebec) "]"
    display "tr [" function locale-date("20261008" turkish) "] [" function locale-time("134512" turkish) "]"
    display "cs [" function locale-date("20261008" czech) "] [" function locale-time("090507" czech) "]"
    display "hu [" function locale-date("20261008" hungarian) "] [" function locale-time("090507" hungarian) "]"
    display "el [" function locale-date("20261008" greek) "] [" function locale-time("134512" greek) "] [" function locale-time("000000" greek) "]"
    display "uk [" function locale-date("20261008" ukrainian) "] [" function locale-time("235959" ukrainian) "]"
    display "posix [" function locale-date("20261008" posix) "] [" function locale-time("134512" posix) "]"
    display "items [" function locale-date(d8 swedish) "] [" function locale-time(t6 us) "]"
    display "national [" function locale-date(n8 german) "]"
    display "seconds [" function locale-time-from-seconds(49512 swedish) "] [" function locale-time-from-seconds(secs us) "] ["
        function locale-time-from-seconds(0 posix) "] [" function locale-time-from-seconds(86399 german) "]"
    set locale lc_time to system-default
    display "current (POSIX) [" function locale-date("20261008") "]"
    set locale lc_time to swedish
    display "current sv [" function locale-date("20261008") "] [" function locale-time("134512") "]"
    set locale lc_collate to german
    display "lc_collate switched, lc_time not [" function locale-date("20261008") "]"
    move function locale-date("20261008" quebec) to r
    display "moved [" r "]"
    display "hour 24 [" function locale-time("240000" posix) "] seconds 99 [" function locale-time("235999" posix) "]"
    display "unchecked bad date [" function locale-date("20261332" posix) "] bad time [" function locale-time("256000" posix)
        "] bad seconds [" function locale-time-from-seconds(86400 posix) "] negative [" function locale-time-from-seconds(-1 posix) "]"
    >>turn ec-argument-function checking on
    move function locale-date("20000230" posix) to r
    display "not reached: EC-ARGUMENT-FUNCTION is fatal, the run unit ends after the declarative (14.6.12)"
    stop run.
