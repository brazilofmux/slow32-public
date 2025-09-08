; Comprehensive test of all i64 operations

; Arithmetic operations
define i64 @test_add(i64 %a, i64 %b) { %c = add i64 %a, %b ret i64 %c }
define i64 @test_sub(i64 %a, i64 %b) { %c = sub i64 %a, %b ret i64 %c }
define i64 @test_mul(i64 %a, i64 %b) { %c = mul i64 %a, %b ret i64 %c }
define i64 @test_udiv(i64 %a, i64 %b) { %c = udiv i64 %a, %b ret i64 %c }
define i64 @test_sdiv(i64 %a, i64 %b) { %c = sdiv i64 %a, %b ret i64 %c }
define i64 @test_urem(i64 %a, i64 %b) { %c = urem i64 %a, %b ret i64 %c }
define i64 @test_srem(i64 %a, i64 %b) { %c = srem i64 %a, %b ret i64 %c }

; Logical operations
define i64 @test_and(i64 %a, i64 %b) { %c = and i64 %a, %b ret i64 %c }
define i64 @test_or(i64 %a, i64 %b) { %c = or i64 %a, %b ret i64 %c }
define i64 @test_xor(i64 %a, i64 %b) { %c = xor i64 %a, %b ret i64 %c }
define i64 @test_shl(i64 %a, i64 %b) { %c = shl i64 %a, %b ret i64 %c }
define i64 @test_lshr(i64 %a, i64 %b) { %c = lshr i64 %a, %b ret i64 %c }
define i64 @test_ashr(i64 %a, i64 %b) { %c = ashr i64 %a, %b ret i64 %c }

; Comparison operations
define i32 @test_eq(i64 %a, i64 %b) { %c = icmp eq i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }
define i32 @test_ne(i64 %a, i64 %b) { %c = icmp ne i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }
define i32 @test_slt(i64 %a, i64 %b) { %c = icmp slt i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }
define i32 @test_sle(i64 %a, i64 %b) { %c = icmp sle i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }
define i32 @test_sgt(i64 %a, i64 %b) { %c = icmp sgt i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }
define i32 @test_sge(i64 %a, i64 %b) { %c = icmp sge i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }
define i32 @test_ult(i64 %a, i64 %b) { %c = icmp ult i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }
define i32 @test_ule(i64 %a, i64 %b) { %c = icmp ule i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }
define i32 @test_ugt(i64 %a, i64 %b) { %c = icmp ugt i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }
define i32 @test_uge(i64 %a, i64 %b) { %c = icmp uge i64 %a, %b %d = zext i1 %c to i32 ret i32 %d }

define i32 @main() {
entry:
  ; === ARITHMETIC TESTS ===
  
  ; Addition with carry
  %add1 = call i64 @test_add(i64 4294967295, i64 1)  ; 0xFFFFFFFF + 1 = 0x100000000
  %check_add1 = icmp eq i64 %add1, 4294967296
  br i1 %check_add1, label %test_sub1, label %fail

test_sub1:
  ; Subtraction with borrow
  %sub1 = call i64 @test_sub(i64 4294967296, i64 1)  ; 0x100000000 - 1 = 0xFFFFFFFF
  %check_sub1 = icmp eq i64 %sub1, 4294967295
  br i1 %check_sub1, label %test_mul1, label %fail

test_mul1:
  ; Multiplication spanning 32-bit boundary
  %mul1 = call i64 @test_mul(i64 65536, i64 65536)  ; 2^16 * 2^16 = 2^32
  %check_mul1 = icmp eq i64 %mul1, 4294967296
  br i1 %check_mul1, label %test_udiv1, label %fail

test_udiv1:
  ; Unsigned division
  %udiv1 = call i64 @test_udiv(i64 1000000000000, i64 1000000)  ; 10^12 / 10^6 = 10^6
  %check_udiv1 = icmp eq i64 %udiv1, 1000000
  br i1 %check_udiv1, label %test_sdiv1, label %fail

test_sdiv1:
  ; Signed division with negative
  %sdiv1 = call i64 @test_sdiv(i64 -1000000000000, i64 1000000)  ; -10^12 / 10^6 = -10^6
  %check_sdiv1 = icmp eq i64 %sdiv1, -1000000
  br i1 %check_sdiv1, label %test_urem1, label %fail

test_urem1:
  ; Unsigned remainder
  %urem1 = call i64 @test_urem(i64 1234567890123, i64 1000000)  ; Should get 890123
  %check_urem1 = icmp eq i64 %urem1, 890123
  br i1 %check_urem1, label %test_srem1, label %fail

test_srem1:
  ; Signed remainder
  %srem1 = call i64 @test_srem(i64 -1234567890123, i64 1000000)  ; Should get -890123
  %check_srem1 = icmp eq i64 %srem1, -890123
  br i1 %check_srem1, label %test_and1, label %fail

  ; === LOGICAL TESTS ===

test_and1:
  ; AND operation
  %and1 = call i64 @test_and(i64 -1, i64 4294967295)  ; 0xFFFFFFFFFFFFFFFF & 0xFFFFFFFF = 0xFFFFFFFF
  %check_and1 = icmp eq i64 %and1, 4294967295
  br i1 %check_and1, label %test_or1, label %fail

test_or1:
  ; OR operation
  %or1 = call i64 @test_or(i64 4294967296, i64 255)  ; 0x100000000 | 0xFF = 0x1000000FF
  %check_or1 = icmp eq i64 %or1, 4294967551
  br i1 %check_or1, label %test_xor1, label %fail

test_xor1:
  ; XOR operation
  %xor1 = call i64 @test_xor(i64 -1, i64 4294967295)  ; 0xFFFFFFFFFFFFFFFF ^ 0xFFFFFFFF = 0xFFFFFFFF00000000
  %check_xor1 = icmp eq i64 %xor1, -4294967296
  br i1 %check_xor1, label %test_shl1, label %fail

test_shl1:
  ; Shift left
  %shl1 = call i64 @test_shl(i64 1, i64 32)  ; 1 << 32 = 0x100000000
  %check_shl1 = icmp eq i64 %shl1, 4294967296
  br i1 %check_shl1, label %test_lshr1, label %fail

test_lshr1:
  ; Logical shift right
  %lshr1 = call i64 @test_lshr(i64 4294967296, i64 32)  ; 0x100000000 >>> 32 = 1
  %check_lshr1 = icmp eq i64 %lshr1, 1
  br i1 %check_lshr1, label %test_ashr1, label %fail

test_ashr1:
  ; Arithmetic shift right (sign extension)
  %ashr1 = call i64 @test_ashr(i64 -4294967296, i64 32)  ; 0xFFFFFFFF00000000 >> 32 = 0xFFFFFFFFFFFFFFFF
  %check_ashr1 = icmp eq i64 %ashr1, -1
  br i1 %check_ashr1, label %test_cmp1, label %fail

  ; === COMPARISON TESTS ===

test_cmp1:
  ; Equal comparison
  %eq1 = call i32 @test_eq(i64 1234567890123, i64 1234567890123)
  %check_eq1 = icmp eq i32 %eq1, 1
  br i1 %check_eq1, label %test_cmp2, label %fail

test_cmp2:
  ; Not equal comparison
  %ne1 = call i32 @test_ne(i64 1234567890123, i64 1234567890124)
  %check_ne1 = icmp eq i32 %ne1, 1
  br i1 %check_ne1, label %test_cmp3, label %fail

test_cmp3:
  ; Signed less than (negative < positive)
  %slt1 = call i32 @test_slt(i64 -1, i64 1)
  %check_slt1 = icmp eq i32 %slt1, 1
  br i1 %check_slt1, label %test_cmp4, label %fail

test_cmp4:
  ; Unsigned less than (large positive > small when viewed as unsigned)
  %ult1 = call i32 @test_ult(i64 1, i64 -1)  ; 1 < 0xFFFFFFFFFFFFFFFF in unsigned
  %check_ult1 = icmp eq i32 %ult1, 1
  br i1 %check_ult1, label %test_cmp5, label %fail

test_cmp5:
  ; Signed greater than
  %sgt1 = call i32 @test_sgt(i64 1, i64 -1)
  %check_sgt1 = icmp eq i32 %sgt1, 1
  br i1 %check_sgt1, label %test_cmp6, label %fail

test_cmp6:
  ; Unsigned greater than
  %ugt1 = call i32 @test_ugt(i64 -1, i64 1)  ; 0xFFFFFFFFFFFFFFFF > 1 in unsigned
  %check_ugt1 = icmp eq i32 %ugt1, 1
  br i1 %check_ugt1, label %test_cmp7, label %fail

test_cmp7:
  ; Less or equal
  %sle1 = call i32 @test_sle(i64 100, i64 100)
  %check_sle1 = icmp eq i32 %sle1, 1
  br i1 %check_sle1, label %test_cmp8, label %fail

test_cmp8:
  ; Greater or equal
  %sge1 = call i32 @test_sge(i64 100, i64 100)
  %check_sge1 = icmp eq i32 %sge1, 1
  br i1 %check_sge1, label %success, label %fail

success:
  ret i32 0

fail:
  ret i32 1
}