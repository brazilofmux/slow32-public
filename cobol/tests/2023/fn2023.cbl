*> The 2023 functions (docs/plans/standard-queue.md item 33): BASECONVERT
*> (15.12), CONCAT (15.18), CONVERT (15.19), FIND-STRING (15.37),
*> MODULE-NAME (15.65: a contained program is part of its outermost
*> program's module), SMALLEST-ALGEBRAIC (15.83), SUBSTITUTE (15.87).
*> No oracle: GnuCOBOL 4 has none of these.
identification division.
program-id. fn2023.
data division.
working-storage section.
01 s1 pic x(20) value "the cat sat on a mat".
01 n1 pic 9(4) value 42.
01 k pic 9(4).
01 hx pic x(4) value "4142".
01 nat1 pic n(3) value n"abc".
01 bits pic 111 value b"101" usage bit.
01 bitd pic 111 value b"101".
01 r pic x(40).
01 sv pic s9(3)v99.
01 sp pic s9pp.
01 sb pic s9(4) comp-5.
01 nest-r pic x(30).
01 nh pic n(4) value n"4142".
01 ns pic n(6) value n"abcabc".
procedure division.
    display "[" function baseconvert("255" 10 16) "]".
    display "[" function baseconvert("FF" 16 2) "]".
    display "[" function baseconvert("777" 8 10) "]".
    display "[" function baseconvert(n1 10 2) "]".
    display "[" function concat("a" "-" "b") "]".
    display "[" function concat(s1(1:3) 42 "/" n1) "]".
    display "[" function concat(function trim("  x  ") function concat("y" "z")) "]".
    display "[" function convert("AB" anum anum hex) "]".
    display "[" function convert(hx hex anum) "]".
    display "[" function convert(hx hex byte) "]".
    display "[" function convert(bits any anum hex) "]" "[" function convert(bitd any anum hex) "]".
    display "[" function convert(n1 any anum hex) "]".
    display "[" function display-of(function convert("hi" anum nat)) "]".
    display "[" function convert(nat1 nat anum) "]".
    display "[" function display-of(function convert("hi" any nat hex)) "]".
    display "[" function find-string(s1 "at") "]".
    display "[" function find-string(s1 "at" last) "]".
    display "[" function find-string(s1 "at" start after 1) "]".
    display "[" function find-string(s1 "AT" anycase) "]".
    display "[" function find-string(s1 "dog") "]".
    compute k = function find-string(s1 "at" 2).
    display "k=" k.
    display "[" function substitute(s1 "at" "og") "]".
    display "[" function substitute(s1 first "at" "og") "]".
    display "[" function substitute(s1 last "at" "og") "]".
    display "[" function substitute(s1 anycase "THE" "a" "cat" "dog") "]".
    display "[" function substitute("aaaa" "a" "bb") "]".
    display "[" function substitute("abc" "b" "") "]".
    display "[" function smallest-algebraic(sv) "]".
    display "[" function smallest-algebraic(sp) "]".
    display "[" function smallest-algebraic(sb) "]".
    display "[" function module-name current "]".
    display "[" function module-name activating "]".
    display "[" function module-name top-level "]".
    display "[" function module-name stack "]".
    call "inner".
    call "nestp".
    display "[" function length(function substitute(s1 "at" "og")) "]".
    move function concat("p" "q") to r.
    display "[" r "]".
*>  LAST is the rightmost occurrence; the national forms
    display "[" function substitute("aaa" last "aa" "X") "]".
    display "[" function substitute("aaa" "aa" "X") "]".
    display "[" function convert(nh hex anum) "]".
    display "[" function find-string(ns n"bc" last) "]".
    display "[" function find-string(ns n"BC" anycase) "]".
    display "[" function display-of(function substitute(ns n"bc" n"Z")) "]".
    display "[" function convert(nh nat anum hex) "]".
    stop run.
identification division.
program-id. nestp.
procedure division.
    display "nested: [" function module-name nested "] [" function module-name current "] [" function module-name activating "] [" function module-name stack "]".
    call "inner".
end program nestp.
end program fn2023.
identification division.
program-id. inner.
procedure division.
    display "inner: [" function module-name current "] [" function module-name activating "] [" function module-name top-level "] [" function module-name stack "]".
end program inner.
