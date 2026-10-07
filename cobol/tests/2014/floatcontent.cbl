*> The 2014 numeric and floating-point class conditions (2023 8.8.4.4.4
*> rules 3g-m) and SET CONTENT OF (14.9.39 format 15): FARTHEST-FROM-ZERO
*> and NEAREST-TO-ZERO of DISPLAY, packed, truncating and capacity-limited
*> binary items (a two's-complement item's extremes differ in magnitude:
*> SIGN required) and of every floating-point format (the largest finite
*> and the smallest subnormal); FLOAT-INFINITY, FLOAT-NOT-A-NUMBER and
*> -SIGNALING set and tested, with SIGN; IN-ARITHMETIC-RANGE (every finite
*> value is within NATIVE's range here: docs/usage.md); the specials shown
*> by DISPLAY as Inf and NaN, not NUMERIC, their sign by the sign bit;
*> infinity's bytes.  No oracle: GnuCOBOL 4 has neither.
*> docs/conformance/conditions.md, set.md
identification division.
program-id. floatcontent.
data division.
working-storage section.
01 p5 pic 9(3)v99.
01 ps pic s9(3)v99.
01 pp pic 9(3)pp.
01 c5 pic s9(4) comp-5.
01 cb pic 9(4) comp.
01 pk pic s9(5) comp-3.
01 b32 usage float-binary-32.
01 b64 usage float-binary-64.
01 b128 usage float-binary-128.
01 d16 usage float-decimal-16.
01 d34 usage float-decimal-34.
01 fl usage float-long.
01 x16 pic x(16).
01 xb redefines x16 usage float-binary-128.
procedure division.
    set content of p5 ps cb pk pp to farthest-from-zero
    display "farthest  " p5 " " ps " " cb " " pk " " pp
    if p5 is farthest-from-zero and cb is farthest-from-zero and pk is farthest-from-zero display "all farthest" end-if
    set content of ps to farthest-from-zero in-arithmetic-range sign negative
    display "negative  " ps
    if ps is farthest-from-zero display "ps farthest either sign" end-if
    set content of p5 ps pk pp to nearest-to-zero sign negative
    display "nearest   " p5 " " ps " " pk " " pp
    if ps is nearest-to-zero and p5 is nearest-to-zero display "nearest either sign" end-if
    if ps is not farthest-from-zero and ps is in-arithmetic-range display "not farthest, in range" end-if
    set content of c5 to farthest-from-zero sign positive display "comp-5    " c5
    if c5 is farthest-from-zero display "comp-5 farthest" end-if
    set content of c5 to farthest-from-zero sign negative display "comp-5    " c5
    if c5 is farthest-from-zero display "comp-5 farthest, negative" end-if
    set content of b32 b64 b128 d16 d34 fl to farthest-from-zero
    display "binary32  " b32
    display "binary64  " b64
    display "binary128 " b128
    display "decimal64 " d16
    display "decimal128 " d34
    display "float-long " fl
    if b32 is farthest-from-zero and b64 is farthest-from-zero and b128 is farthest-from-zero and fl is farthest-from-zero display "binaries farthest" end-if
    if d16 is farthest-from-zero and d34 is farthest-from-zero display "decimals farthest" end-if
    if b64 is in-arithmetic-range and d34 is in-arithmetic-range display "in range" end-if
    set content of b32 b64 b128 d16 d34 to nearest-to-zero
    display "binary32  " b32
    display "binary64  " b64
    display "binary128 " b128
    display "decimal64 " d16
    display "decimal128 " d34
    if b32 is nearest-to-zero and b64 is nearest-to-zero and b128 is nearest-to-zero display "binaries nearest" end-if
    if d16 is nearest-to-zero and d34 is nearest-to-zero display "decimals nearest" end-if
    set content of b128 to float-infinity sign negative
    display "infinity  " b128
    if b128 is float-infinity and b128 is negative display "negative infinity" end-if
    if b128 is not numeric and b128 is not in-arithmetic-range display "not numeric, not in range" end-if
    set content of d16 to float-not-a-number
    display "nan       " d16
    if d16 is float-not-a-number and d16 is float-not-a-number-quiet and not d16 is float-not-a-number-signaling display "quiet nan" end-if
    set content of d16 to float-not-a-number-signaling sign negative
    display "nan       " d16
    if d16 is float-not-a-number and d16 is float-not-a-number-signaling and d16 is negative display "signaling nan, negative" end-if
    set content of fl to float-infinity
    display "float-long " fl
    set content of fl to float-not-a-number-signaling
    if fl is float-not-a-number-signaling and fl is not numeric display "float-long signaling nan" end-if
    set content of b32 to float-not-a-number
    if b32 is float-not-a-number-quiet display "binary32 quiet nan" end-if
    set content of xb to float-infinity
    if x16 = x"0000000000000000000000000000ff7f" display "infinity bytes" end-if
    set content of xb to float-not-a-number
    if x16 = x"0000000000000000000000000000ff7f" display "x" else display "nan bytes differ" end-if
    stop run.
