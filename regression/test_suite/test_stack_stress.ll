; Test stack frame management under stress
; Deep recursion, large stack allocations, multiple saved registers

define i32 @fibonacci(i32 %n) {
entry:
  ; Test recursive calls with stack frame management
  %cmp_zero = icmp eq i32 %n, 0
  br i1 %cmp_zero, label %return_zero, label %check_one

check_one:
  %cmp_one = icmp eq i32 %n, 1
  br i1 %cmp_one, label %return_one, label %recurse

return_zero:
  ret i32 0

return_one:
  ret i32 1

recurse:
  ; Compute fib(n-1) and fib(n-2)
  %n_minus_1 = sub i32 %n, 1
  %n_minus_2 = sub i32 %n, 2
  
  %fib_n_minus_1 = call i32 @fibonacci(i32 %n_minus_1)
  %fib_n_minus_2 = call i32 @fibonacci(i32 %n_minus_2)
  
  %result = add i32 %fib_n_minus_1, %fib_n_minus_2
  ret i32 %result
}

define i32 @many_locals() {
entry:
  ; Allocate many local variables to stress stack allocation
  %local1 = alloca i32, align 4
  %local2 = alloca i32, align 4
  %local3 = alloca i32, align 4
  %local4 = alloca i32, align 4
  %local5 = alloca i32, align 4
  %local6 = alloca i32, align 4
  %local7 = alloca i32, align 4
  %local8 = alloca i32, align 4
  %local9 = alloca i32, align 4
  %local10 = alloca i32, align 4
  
  ; Initialize all locals
  store i32 1, ptr %local1
  store i32 2, ptr %local2
  store i32 3, ptr %local3
  store i32 4, ptr %local4
  store i32 5, ptr %local5
  store i32 6, ptr %local6
  store i32 7, ptr %local7
  store i32 8, ptr %local8
  store i32 9, ptr %local9
  store i32 10, ptr %local10
  
  ; Sum all locals
  %v1 = load i32, ptr %local1
  %v2 = load i32, ptr %local2
  %v3 = load i32, ptr %local3
  %v4 = load i32, ptr %local4
  %v5 = load i32, ptr %local5
  %v6 = load i32, ptr %local6
  %v7 = load i32, ptr %local7
  %v8 = load i32, ptr %local8
  %v9 = load i32, ptr %local9
  %v10 = load i32, ptr %local10
  
  %sum1 = add i32 %v1, %v2
  %sum2 = add i32 %sum1, %v3
  %sum3 = add i32 %sum2, %v4
  %sum4 = add i32 %sum3, %v5
  %sum5 = add i32 %sum4, %v6
  %sum6 = add i32 %sum5, %v7
  %sum7 = add i32 %sum6, %v8
  %sum8 = add i32 %sum7, %v9
  %sum9 = add i32 %sum8, %v10
  
  ret i32 %sum9  ; Should be 55
}

define i32 @nested_with_many_args(i32 %a1, i32 %a2, i32 %a3, i32 %a4,
                                  i32 %a5, i32 %a6, i32 %a7, i32 %a8,
                                  i32 %a9, i32 %a10, i32 %a11, i32 %a12) {
entry:
  ; Test passing many arguments through nested calls
  ; Also tests saving/restoring many registers
  
  ; Do some computation with all arguments
  %sum1 = add i32 %a1, %a2
  %sum2 = add i32 %sum1, %a3
  %sum3 = add i32 %sum2, %a4
  %sum4 = add i32 %sum3, %a5
  %sum5 = add i32 %sum4, %a6
  %sum6 = add i32 %sum5, %a7
  %sum7 = add i32 %sum6, %a8
  %sum8 = add i32 %sum7, %a9
  %sum9 = add i32 %sum8, %a10
  %sum10 = add i32 %sum9, %a11
  %sum11 = add i32 %sum10, %a12
  
  ; Call another function in the middle
  %temp = call i32 @many_locals()
  
  ; Use the result
  %final = add i32 %sum11, %temp
  
  ret i32 %final
}

define i32 @deep_chain_5(i32 %x) {
entry:
  %result = add i32 %x, 5
  ret i32 %result
}

define i32 @deep_chain_4(i32 %x) {
entry:
  %result = call i32 @deep_chain_5(i32 %x)
  %final = add i32 %result, 4
  ret i32 %final
}

define i32 @deep_chain_3(i32 %x) {
entry:
  %result = call i32 @deep_chain_4(i32 %x)
  %final = add i32 %result, 3
  ret i32 %final
}

define i32 @deep_chain_2(i32 %x) {
entry:
  %result = call i32 @deep_chain_3(i32 %x)
  %final = add i32 %result, 2
  ret i32 %final
}

define i32 @deep_chain_1(i32 %x) {
entry:
  %result = call i32 @deep_chain_2(i32 %x)
  %final = add i32 %result, 1
  ret i32 %final
}

define i32 @main() {
entry:
  ; Test fibonacci(10) = 55
  %fib10 = call i32 @fibonacci(i32 10)
  %cmp1 = icmp eq i32 %fib10, 55
  br i1 %cmp1, label %test2, label %fail

test2:
  ; Test many locals
  %result2 = call i32 @many_locals()
  %cmp2 = icmp eq i32 %result2, 55
  br i1 %cmp2, label %test3, label %fail

test3:
  ; Test nested with many args
  ; 1+2+3+...+12 = 78, plus 55 from many_locals = 133
  %result3 = call i32 @nested_with_many_args(i32 1, i32 2, i32 3, i32 4,
                                              i32 5, i32 6, i32 7, i32 8,
                                              i32 9, i32 10, i32 11, i32 12)
  %cmp3 = icmp eq i32 %result3, 133
  br i1 %cmp3, label %test4, label %fail

test4:
  ; Test deep call chain: 10 + 1 + 2 + 3 + 4 + 5 = 25
  %result4 = call i32 @deep_chain_1(i32 10)
  %cmp4 = icmp eq i32 %result4, 25
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