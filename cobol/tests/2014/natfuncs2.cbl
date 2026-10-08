identification division.
program-id. natfuncs2.
*> The intrinsic functions that take a national argument through the
*> 1989 table -- ORD, REVERSE, NUMVAL, NUMVAL-C, NUMVAL-F, TEST-NUMVAL
*> (15.70, 15.79, 15.67-15.69, 15.93) -- and TRIM, UPPER-CASE and
*> LOWER-CASE on supplementary characters (surrogate pairs).  Found by
*> the national audit of 2026-10-08: ORD read one byte of the two, REVERSE
*> reversed the bytes and called its result alphanumeric, and the NUMVAL
*> family scanned the UTF-16 bytes and read zero.  ORD is the code unit
*> plus one; REVERSE keeps a surrogate pair in its order; a parted pair
*> displays as U+FFFD.  No oracle: GnuCOBOL's national data is not UTF-16 (docs/national.md).
data division.
working-storage section.
01 n4 pic n(4) value n"a😀b".
01 n3 pic n(3).
01 n5 pic n(5).
01 nn pic n(6) value n" -12.5".
01 r pic -(5)9.99.
01 o pic 9(5).
procedure division.
    display "rev   [" function reverse(n4) "] " function length(function reverse(n4))
    display "rev2  [" function reverse(n"😀😀") "]"
    move function ord(n"a") to o display "ord a " o
    move function ord(n"é") to o display "ord e " o
    move function ord(n4(2:1)) to o display "ord hi " o
    move function ord("a") to o display "ord x " o
    compute r = function numval(nn) display "nvl   " r
    compute r = function numval-c(n"$1,234.5") display "nvc   " r
    compute r = function numval-c(n"€7", n"€") display "nvc2  " r
    compute r = function numval-f(n"-1.5E+2") display "nvf   " r
    display "tnv   " function test-numval(nn)
    display "tnv2  " function test-numval(n"12x")
    display "tnv3  " function test-numval(n"1😀")
    display "tnvc  " function test-numval-c(n"$1,2x4")
    display "tnvf  " function test-numval-f(n"1.5E2")
*> Deseret letters have a case; the pair stays two positions
    move n"𐐨a𐐩" to n5
    display "up    " function upper-case(n5)
    display "low   " function lower-case(function upper-case(n5))
    display "trim  [" function trim(n"  😀 a  ") "] " function length(function trim(n"  😀 a  "))
    display "trimL [" function trim(n"  😀 a  " leading) "]"
    display "trimT [" function trim(n"  😀 a  " trailing) "]"
    display "trim0 [" function trim(n"   ") "] " function length(function trim(n"   "))
*> truncation by positions parts a pair; each lone surrogate shows as U+FFFD
    move n"ab😀" to n3
    display "part  [" n3 "] " function length(n3)
    move n"😀😀" to n3
    display "part2 [" n3(3:1) "|" n3(2:2) "]"
    goback.
