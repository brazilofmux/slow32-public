; Test i64 arithmetic operations (add/sub with carry/borrow)

define i64 @test_add_simple(i64 %a, i64 %b) {
  %c = add i64 %a, %b
  ret i64 %c
}

define i64 @test_sub_simple(i64 %a, i64 %b) {
  %c = sub i64 %a, %b
  ret i64 %c
}

define i32 @main() {
entry:
  ; Test 1: Simple add without carry (100 + 200 = 300)
  %a1 = call i64 @test_add_simple(i64 100, i64 200)
  %check1 = icmp eq i64 %a1, 300
  br i1 %check1, label %test2, label %fail

test2:
  ; Test 2: Add with carry propagation (0xFFFFFFFF + 1 = 0x100000000)
  %a2 = call i64 @test_add_simple(i64 4294967295, i64 1)
  %check2 = icmp eq i64 %a2, 4294967296
  br i1 %check2, label %test3, label %fail

test3:
  ; Test 3: Add that wraps around (0xFFFFFFFFFFFFFFFF + 1 = 0)
  %a3 = call i64 @test_add_simple(i64 -1, i64 1)
  %check3 = icmp eq i64 %a3, 0
  br i1 %check3, label %test4, label %fail

test4:
  ; Test 4: Simple subtraction (500 - 200 = 300)
  %s1 = call i64 @test_sub_simple(i64 500, i64 200)
  %check4 = icmp eq i64 %s1, 300
  br i1 %check4, label %test5, label %fail

test5:
  ; Test 5: Subtraction with borrow (0x100000000 - 1 = 0xFFFFFFFF)
  %s2 = call i64 @test_sub_simple(i64 4294967296, i64 1)
  %check5 = icmp eq i64 %s2, 4294967295
  br i1 %check5, label %test6, label %fail

test6:
  ; Test 6: Subtraction that underflows (0 - 1 = 0xFFFFFFFFFFFFFFFF)
  %s3 = call i64 @test_sub_simple(i64 0, i64 1)
  %check6 = icmp eq i64 %s3, -1
  br i1 %check6, label %test7, label %fail

test7:
  ; Test 7: Large numbers addition
  ; 0x123456789ABCDEF0 + 0x0FEDCBA987654321 = 0x2222222222222211
  %a4 = call i64 @test_add_simple(i64 1311768467463790320, i64 1147797409030816545)
  %check7 = icmp eq i64 %a4, 2459565876494606865
  br i1 %check7, label %pass, label %fail

pass:
  ret i32 0  ; Success

fail:
  ret i32 1  ; Failure
}