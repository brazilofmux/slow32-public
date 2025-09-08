; Test function calls, stack frames, and >8 arguments
; Validates JAL/JALR, prologue/epilogue, and argument passing

define i32 @simple_add(i32 %a, i32 %b) {
entry:
  %sum = add i32 %a, %b
  ret i32 %sum
}

define i32 @recursive_factorial(i32 %n) {
entry:
  %cmp = icmp eq i32 %n, 0
  br i1 %cmp, label %base_case, label %recursive_case

base_case:
  ret i32 1

recursive_case:
  %n_minus_1 = sub i32 %n, 1
  %fact_n_minus_1 = call i32 @recursive_factorial(i32 %n_minus_1)
  %result = mul i32 %n, %fact_n_minus_1
  ret i32 %result
}

; Test function with more than 8 arguments (tests stack passing)
define i32 @many_args(i32 %a1, i32 %a2, i32 %a3, i32 %a4, 
                      i32 %a5, i32 %a6, i32 %a7, i32 %a8,
                      i32 %a9, i32 %a10) {
entry:
  ; Sum first 8 args (passed in registers)
  %sum1 = add i32 %a1, %a2
  %sum2 = add i32 %sum1, %a3
  %sum3 = add i32 %sum2, %a4
  %sum4 = add i32 %sum3, %a5
  %sum5 = add i32 %sum4, %a6
  %sum6 = add i32 %sum5, %a7
  %sum7 = add i32 %sum6, %a8
  
  ; Add args 9 and 10 (passed on stack)
  %sum8 = add i32 %sum7, %a9
  %sum9 = add i32 %sum8, %a10
  
  ret i32 %sum9
}

; Test deeply nested calls (validates stack frame preservation)
define i32 @level3(i32 %x) {
entry:
  %doubled = mul i32 %x, 2
  ret i32 %doubled
}

define i32 @level2(i32 %x) {
entry:
  %plus_5 = add i32 %x, 5
  %result = call i32 @level3(i32 %plus_5)
  ret i32 %result
}

define i32 @level1(i32 %x) {
entry:
  %plus_10 = add i32 %x, 10
  %result = call i32 @level2(i32 %plus_10)
  ret i32 %result
}

define i32 @main() {
entry:
  ; Test simple function call
  %result1 = call i32 @simple_add(i32 15, i32 25)
  %cmp1 = icmp eq i32 %result1, 40
  br i1 %cmp1, label %test2, label %fail

test2:
  ; Test recursive call (5! = 120)
  %result2 = call i32 @recursive_factorial(i32 5)
  %cmp2 = icmp eq i32 %result2, 120
  br i1 %cmp2, label %test3, label %fail

test3:
  ; Test >8 arguments (1+2+3+4+5+6+7+8+9+10 = 55)
  %result3 = call i32 @many_args(i32 1, i32 2, i32 3, i32 4, i32 5,
                                  i32 6, i32 7, i32 8, i32 9, i32 10)
  %cmp3 = icmp eq i32 %result3, 55
  br i1 %cmp3, label %test4, label %fail

test4:
  ; Test nested calls: level1(5) = level2(15) = level3(20) = 40
  %result4 = call i32 @level1(i32 5)
  %cmp4 = icmp eq i32 %result4, 40
  br i1 %cmp4, label %pass, label %fail

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