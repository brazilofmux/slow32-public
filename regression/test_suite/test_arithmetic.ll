; Test basic arithmetic operations
; Expected: Should compute (10 + 20) * 3 - 15 / 5 = 90 - 3 = 87

define i32 @test_arithmetic() {
entry:
  ; Basic addition
  %sum = add i32 10, 20        ; 30
  
  ; Multiplication
  %prod = mul i32 %sum, 3      ; 90
  
  ; Division
  %div = sdiv i32 15, 5        ; 3
  
  ; Subtraction
  %result = sub i32 %prod, %div ; 87
  
  ret i32 %result
}

define i32 @test_remainder() {
entry:
  ; Test remainder operation
  %rem1 = srem i32 17, 5       ; 2
  %rem2 = srem i32 23, 7       ; 2
  %sum = add i32 %rem1, %rem2  ; 4
  ret i32 %sum
}

define i32 @main() {
entry:
  %result1 = call i32 @test_arithmetic()
  %cmp1 = icmp eq i32 %result1, 87
  br i1 %cmp1, label %test2, label %fail

test2:
  %result2 = call i32 @test_remainder()
  %cmp2 = icmp eq i32 %result2, 4
  br i1 %cmp2, label %pass, label %fail

pass:
  ; Output "PASS\n"
  call void @debug_char(i32 80)  ; 'P'
  call void @debug_char(i32 65)  ; 'A'
  call void @debug_char(i32 83)  ; 'S'
  call void @debug_char(i32 83)  ; 'S'
  call void @debug_char(i32 10)  ; '\n'
  ret i32 0

fail:
  ; Output "FAIL\n"
  call void @debug_char(i32 70)  ; 'F'
  call void @debug_char(i32 65)  ; 'A'
  call void @debug_char(i32 73)  ; 'I'
  call void @debug_char(i32 76)  ; 'L'
  call void @debug_char(i32 10)  ; '\n'
  ret i32 1
}

declare void @debug_char(i32)