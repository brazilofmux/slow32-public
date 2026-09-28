identification division.
program-id. natcase.
*> UPPER-CASE and LOWER-CASE on national and UTF-8 text (COBOL 2002
*> 15.78, 15.52, Annex D; cobol ISSUES-66): Unicode's simple case
*> mappings, from UnicodeData.txt as Annex D note 1 advises.  With no
*> locale the result is the argument's length (E.13.2.4), so a mapping
*> is one-to-one: sharp s has no one-character uppercase and stays;
*> final sigma becomes capital sigma; dotless i becomes I, and capital
*> I with a dot becomes i.  A supplementary letter, two national
*> character positions (Deseret, U+10428), maps as one character.  In
*> alphanumeric text a letter is mapped when its other case has the
*> same number of UTF-8 bytes -- e-acute does (two and two); dotless i
*> (two bytes; I is one) and U+2C65 (three; U+023A is two) do not -- and
*> a byte that begins no UTF-8 character is left alone.  U+6162 is
*> two bytes that spell "ab" and is not a letter with a case.  No oracle
*> (docs/national.md).
data division.
working-storage section.
01  n        pic n(12).
01  a        pic x(12).
01  bad      pic x(4) value "ab".
procedure division.
main.
    move function upper-case(n"straße café") to n
    display "national upper: [" n "]"
    move function lower-case(n"ÀÉÎ ΣΑΣ İI") to n
    display "national lower: [" n "]"
    move function upper-case(n"a慢b") to n
    display "a CJK character whose bytes are letters: [" n "]"
    move function upper-case(n"σς ı 𐐨x") to n
    display "sigma, dotless, Deseret: [" n "]"
    move function lower-case(function upper-case(n"𐐨")) to n
    display "Deseret round trip: [" n "] " function length(function upper-case(n"𐐨"))
    move function upper-case("café") to a
    display "alphanumeric: [" a "]"
    move function upper-case("ı ⱥ z") to a
    display "not the same width: [" a "]"
    move x"E9" to bad(3:1)
    move function upper-case(bad) to a
    if a(1:3) = x"4142E9" display "a Latin-1 byte: unchanged" end-if
    move function upper-case(function national-of("naïve")) to n
    display "of a national-of: [" n "] " function length(function upper-case(function national-of("naïve")))
    stop run.
