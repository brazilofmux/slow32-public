; Test i64 division and remainder operations

; Test functions for division
define i64 @test_udiv(i64 %a, i64 %b) {
  %c = udiv i64 %a, %b
  ret i64 %c
}

define i64 @test_sdiv(i64 %a, i64 %b) {
  %c = sdiv i64 %a, %b
  ret i64 %c
}

define i64 @test_urem(i64 %a, i64 %b) {
  %c = urem i64 %a, %b
  ret i64 %c
}

define i64 @test_srem(i64 %a, i64 %b) {
  %c = srem i64 %a, %b
  ret i64 %c
}

define i32 @main() {
entry:
  ; Test 1: Simple unsigned division (1000000 / 1000 = 1000)
  %t1 = call i64 @test_udiv(i64 1000000, i64 1000)
  %check1 = icmp eq i64 %t1, 1000
  br i1 %check1, label %test2, label %fail

test2:
  ; Test 2: Unsigned division with 64-bit values (10^12 / 10^6 = 10^6)
  %t2 = call i64 @test_udiv(i64 1000000000000, i64 1000000)
  %check2 = icmp eq i64 %t2, 1000000
  br i1 %check2, label %test3, label %fail

test3:
  ; Test 3: Signed division with negative dividend (-1000000 / 1000 = -1000)
  %t3 = call i64 @test_sdiv(i64 -1000000, i64 1000)
  %check3 = icmp eq i64 %t3, -1000
  br i1 %check3, label %test4, label %fail

test4:
  ; Test 4: Signed division with negative divisor (1000000 / -1000 = -1000)
  %t4 = call i64 @test_sdiv(i64 1000000, i64 -1000)
  %check4 = icmp eq i64 %t4, -1000
  br i1 %check4, label %test5, label %fail

test5:
  ; Test 5: Signed division with both negative (-1000000 / -1000 = 1000)
  %t5 = call i64 @test_sdiv(i64 -1000000, i64 -1000)
  %check5 = icmp eq i64 %t5, 1000
  br i1 %check5, label %test6, label %fail

test6:
  ; Test 6: Unsigned remainder (1234567 % 1000 = 567)
  %t6 = call i64 @test_urem(i64 1234567, i64 1000)
  %check6 = icmp eq i64 %t6, 567
  br i1 %check6, label %test7, label %fail

test7:
  ; Test 7: Unsigned remainder with large values (10^12 + 123 % 10^6 = 123)
  %t7 = call i64 @test_urem(i64 1000000000123, i64 1000000)
  %check7 = icmp eq i64 %t7, 123
  br i1 %check7, label %test8, label %fail

test8:
  ; Test 8: Signed remainder with negative dividend (-1234567 % 1000 = -567)
  %t8 = call i64 @test_srem(i64 -1234567, i64 1000)
  %check8 = icmp eq i64 %t8, -567
  br i1 %check8, label %test9, label %fail

test9:
  ; Test 9: Signed remainder with negative divisor (1234567 % -1000 = 567)
  %t9 = call i64 @test_srem(i64 1234567, i64 -1000)
  %check9 = icmp eq i64 %t9, 567
  br i1 %check9, label %test10, label %fail

test10:
  ; Test 10: Division by power of 2 (should optimize to shift)
  %t10 = call i64 @test_udiv(i64 65536, i64 256)
  %check10 = icmp eq i64 %t10, 256
  br i1 %check10, label %test11, label %fail

test11:
  ; Test 11: Remainder by power of 2 (should optimize to mask)
  %t11 = call i64 @test_urem(i64 65537, i64 256)
  %check11 = icmp eq i64 %t11, 1
  br i1 %check11, label %test12, label %fail

test12:
  ; Test 12: Edge case - division by 1
  %t12 = call i64 @test_udiv(i64 123456789, i64 1)
  %check12 = icmp eq i64 %t12, 123456789
  br i1 %check12, label %test13, label %fail

test13:
  ; Test 13: Edge case - 0 divided by anything
  %t13 = call i64 @test_udiv(i64 0, i64 12345)
  %check13 = icmp eq i64 %t13, 0
  br i1 %check13, label %test14, label %fail

test14:
  ; Test 14: Large 64-bit division (2^63 / 2^32)
  %t14 = call i64 @test_udiv(i64 9223372036854775808, i64 4294967296)
  %check14 = icmp eq i64 %t14, 2147483648
  br i1 %check14, label %success, label %fail

success:
  ret i32 0

fail:
  ret i32 1
}