; Test i64 multiplication operations

define i64 @test_mul(i64 %a, i64 %b) {
  %c = mul i64 %a, %b
  ret i64 %c
}

define i32 @main() {
entry:
  ; Test 1: Simple multiplication (100 * 200 = 20000)
  %t1 = call i64 @test_mul(i64 100, i64 200)
  %check1 = icmp eq i64 %t1, 20000
  br i1 %check1, label %test2, label %fail

test2:
  ; Test 2: Multiplication with larger values (1000000 * 1000 = 1000000000)
  %t2 = call i64 @test_mul(i64 1000000, i64 1000)
  %check2 = icmp eq i64 %t2, 1000000000
  br i1 %check2, label %test3, label %fail

test3:
  ; Test 3: Multiplication by 0
  %t3 = call i64 @test_mul(i64 123456789, i64 0)
  %check3 = icmp eq i64 %t3, 0
  br i1 %check3, label %test4, label %fail

test4:
  ; Test 4: Multiplication by 1
  %t4 = call i64 @test_mul(i64 987654321, i64 1)
  %check4 = icmp eq i64 %t4, 987654321
  br i1 %check4, label %test5, label %fail

test5:
  ; Test 5: Multiplication by -1
  %t5 = call i64 @test_mul(i64 123456789, i64 -1)
  %check5 = icmp eq i64 %t5, -123456789
  br i1 %check5, label %test6, label %fail

test6:
  ; Test 6: Negative * Negative = Positive
  %t6 = call i64 @test_mul(i64 -1000, i64 -2000)
  %check6 = icmp eq i64 %t6, 2000000
  br i1 %check6, label %test7, label %fail

test7:
  ; Test 7: Power of 2 multiplication (should optimize to shift)
  %t7 = call i64 @test_mul(i64 12345, i64 256)
  %check7 = icmp eq i64 %t7, 3160320
  br i1 %check7, label %test8, label %fail

test8:
  ; Test 8: Large multiplication that uses high 32 bits
  ; 0x100000 * 0x100000 = 0x10000000000 (uses 40 bits)
  %t8 = call i64 @test_mul(i64 1048576, i64 1048576)
  %check8 = icmp eq i64 %t8, 1099511627776
  br i1 %check8, label %test9, label %fail

test9:
  ; Test 9: Multiplication that spans both 32-bit halves
  ; 0xFFFFFFFF * 2 = 0x1FFFFFFFE
  %t9 = call i64 @test_mul(i64 4294967295, i64 2)
  %check9 = icmp eq i64 %t9, 8589934590
  br i1 %check9, label %test10, label %fail

test10:
  ; Test 10: Square a 32-bit value
  ; 65536^2 = 4294967296 (exactly 2^32)
  %t10 = call i64 @test_mul(i64 65536, i64 65536)
  %check10 = icmp eq i64 %t10, 4294967296
  br i1 %check10, label %test11, label %fail

test11:
  ; Test 11: Mixed signs with carry
  %t11 = call i64 @test_mul(i64 -1000000, i64 1000000)
  %check11 = icmp eq i64 %t11, -1000000000000
  br i1 %check11, label %test12, label %fail

test12:
  ; Test 12: Test overflow wrapping (result truncated to 64 bits)
  ; This multiplication would overflow but we just get the low 64 bits
  %t12 = call i64 @test_mul(i64 9223372036854775807, i64 2)
  %check12 = icmp eq i64 %t12, -2
  br i1 %check12, label %success, label %fail

success:
  ret i32 0

fail:
  ret i32 1
}