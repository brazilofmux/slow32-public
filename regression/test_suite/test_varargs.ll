; Test varargs support with SLOW32's char* va_list
; Now that clang properly generates SLOW32 varargs, this should work

define i32 @sum_ints(i32 %count, ...) {
entry:
  %count.addr = alloca i32, align 4
  store i32 %count, ptr %count.addr
  
  %va = alloca ptr, align 4
  call void @llvm.va_start.p0(ptr %va)
  
  %sum = alloca i32, align 4
  store i32 0, ptr %sum
  
  %i = alloca i32, align 4
  store i32 0, ptr %i
  
  br label %loop

loop:
  %i_val = load i32, ptr %i
  %count_val = load i32, ptr %count.addr
  %cmp = icmp slt i32 %i_val, %count_val
  br i1 %cmp, label %body, label %done

body:
  ; Get next vararg
  %arg = va_arg ptr %va, i32
  
  ; Add to sum
  %old_sum = load i32, ptr %sum
  %new_sum = add i32 %old_sum, %arg
  store i32 %new_sum, ptr %sum
  
  ; Increment counter
  %old_i = load i32, ptr %i
  %new_i = add i32 %old_i, 1
  store i32 %new_i, ptr %i
  
  br label %loop

done:
  call void @llvm.va_end.p0(ptr %va)
  %result = load i32, ptr %sum
  ret i32 %result
}

; Test with different numbers of arguments
define i32 @test_3_args() {
entry:
  %result = call i32 (i32, ...) @sum_ints(i32 3, i32 10, i32 20, i32 30)
  ret i32 %result  ; Should be 60
}

define i32 @test_5_args() {
entry:
  %result = call i32 (i32, ...) @sum_ints(i32 5, i32 1, i32 2, i32 3, i32 4, i32 5)
  ret i32 %result  ; Should be 15
}

define i32 @test_10_args() {
entry:
  ; Test with more than 8 varargs (some will be on stack)
  %result = call i32 (i32, ...) @sum_ints(i32 10, 
                                          i32 1, i32 2, i32 3, i32 4, i32 5,
                                          i32 6, i32 7, i32 8, i32 9, i32 10)
  ret i32 %result  ; Should be 55
}

define i32 @main() {
entry:
  ; Test 3 arguments
  %result1 = call i32 @test_3_args()
  %cmp1 = icmp eq i32 %result1, 60
  br i1 %cmp1, label %test2, label %fail

test2:
  ; Test 5 arguments
  %result2 = call i32 @test_5_args()
  %cmp2 = icmp eq i32 %result2, 15
  br i1 %cmp2, label %test3, label %fail

test3:
  ; Test 10 arguments (more than register count)
  %result3 = call i32 @test_10_args()
  %cmp3 = icmp eq i32 %result3, 55
  br i1 %cmp3, label %pass, label %fail

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

declare void @llvm.va_start.p0(ptr)
declare void @llvm.va_end.p0(ptr)
declare void @debug_char(i32)