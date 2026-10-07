*> The standard floating-point usages (COBOL 2014; 2023 13.18.60.4 rules
*> 14-18, 11.9.8-9) beyond what GnuCOBOL has: FLOAT-BINARY-32/64 (the
*> hardware's float and double, shown in the MF form FLOAT-SHORT/-LONG
*> use), FLOAT-BINARY-128 in software (binary128, correctly rounded both
*> ways), DISPLAY of the software formats in the implementor's form
*> (significant digits, one before the point, a decimal exponent); the
*> endianness and encoding phrases and the OPTIONS defaults, checked by
*> the bytes (HIGH-ORDER-LEFT reverses them; DECIMAL-ENCODING is DPD);
*> a size error past a format (GnuCOBOL stores an infinity instead); a
*> double beside a decimal128 computes as decimals (the double read
*> exactly); an infinity in the bytes is not NUMERIC; the sign condition
*> by the sign bit.  No oracle: GnuCOBOL 4 has no FLOAT-BINARY usages.
*> docs/conformance/usage.md
identification division.
program-id. floatbin.
environment division.
configuration section.
procedure division.
    call "inner"
    stop run.
identification division.
program-id. inner.
options.
    float-decimal default is decimal-encoding high-order-left.
data division.
working-storage section.
01 b32 usage float-binary-32 value 1.5.
01 b64 usage float-binary-64 value 0.1.
01 b128 usage float-binary-128 value 0.1.
01 bl usage float-binary-64 high-order-left value 1.
01 x8 pic x(8).
01 bx redefines x8 usage float-binary-64.
01 blx redefines x8 usage float-binary-64 high-order-left.
01 dxr redefines x8 usage float-decimal-16 binary-encoding high-order-right.
01 d16 usage float-decimal-16 value 1.
01 d16r redefines d16 pic x(8).
01 d16b usage float-decimal-16 binary-encoding high-order-right value 1.
01 d16br redefines d16b pic x(8).
01 d34 usage float-decimal-34 value 0.1.
01 x16 pic x(16).
01 qx redefines x16 usage float-binary-128.
01 p pic 9(3)v9(28).
procedure division.
    display "b32  " b32
    display "b64  " b64
    display "b128 " b128
    display "d16  " d16
    display "d34  " d34
    move 1 to bx
    if x8 = x"000000000000f03f" display "binary64 bytes, low order first" end-if
    move 1 to blx
    if x8 = x"3ff0000000000000" display "HIGH-ORDER-LEFT: high order first" end-if
    if bl = blx display "the same value either way" end-if
    move 1 to qx
    if x16(16:1) = x"3f" and x16(15:1) = x"ff" and x16(1:1) = x"00" display "binary128 bytes, low order first" end-if
    if d16r = x"2238000000000001" display "DPD, high order first: the OPTIONS default" end-if
    if d16br = x"010000000000c031" display "BID, low order first: the item's own phrases" end-if
    compute b128 = b128 + 0.2 display "0.1 + 0.2 in binary128: " b128
    compute b128 = 1 / 3 display "1 / 3 in binary128:     " b128
    move b128 to p display "to a picture:            " p
    compute d34 = b64 + 0.2 display "a double plus 0.2, as decimals: " d34
    compute d16 = 10 ** 300 * 10 ** 100
        on size error display "size error past decimal64"
    end-compute
    compute b128 = 10 ** 30 ** 200
        on size error display "size error past binary128"
    end-compute
    compute b128 = 10 ** 30 ** 100 display "10 ** 3000 in binary128: " b128
    compute b128 = 1 / b128 display "its reciprocal:          " b128
    move x"000000000000f07f" to x8
    if bx is numeric display "x" else display "an infinity is not NUMERIC" end-if
    move x"0000000000000000000000000000ff7f" to x16
    if qx is numeric display "x" else display "a binary128 infinity is not NUMERIC" end-if
    move x"000000000000c0b1" to x8
    if dxr is negative display "-0 is NEGATIVE by its sign bit" end-if
    if dxr is zero display "and ZERO" end-if
    move -1.5 to b128
    if b128 is negative display "negative binary128" end-if
    goback.
end program inner.
end program floatbin.
