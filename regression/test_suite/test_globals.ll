; Test global variable addressing and static data
; Validates global variable access patterns

@global_counter = global i32 0
@global_array = global [5 x i32] [i32 10, i32 20, i32 30, i32 40, i32 50]
@global_const = constant i32 42

; Test string constant (for future string support)
@hello_string = constant [6 x i8] c"Hello\00"

define i32 @test_global_read() {
entry:
  ; Read from global constant
  %const_val = load i32, ptr @global_const
  ret i32 %const_val
}

define void @test_global_write(i32 %value) {
entry:
  ; Write to global variable
  store i32 %value, ptr @global_counter
  ret void
}

define i32 @test_global_increment() {
entry:
  ; Read-modify-write pattern
  %old_val = load i32, ptr @global_counter
  %new_val = add i32 %old_val, 1
  store i32 %new_val, ptr @global_counter
  ret i32 %new_val
}

define i32 @test_global_array_access(i32 %index) {
entry:
  ; Array element access using getelementptr
  %ptr = getelementptr [5 x i32], ptr @global_array, i32 0, i32 %index
  %value = load i32, ptr %ptr
  ret i32 %value
}

define i32 @sum_global_array() {
entry:
  ; Sum all elements of global array
  %ptr0 = getelementptr [5 x i32], ptr @global_array, i32 0, i32 0
  %val0 = load i32, ptr %ptr0
  
  %ptr1 = getelementptr [5 x i32], ptr @global_array, i32 0, i32 1
  %val1 = load i32, ptr %ptr1
  
  %ptr2 = getelementptr [5 x i32], ptr @global_array, i32 0, i32 2
  %val2 = load i32, ptr %ptr2
  
  %ptr3 = getelementptr [5 x i32], ptr @global_array, i32 0, i32 3
  %val3 = load i32, ptr %ptr3
  
  %ptr4 = getelementptr [5 x i32], ptr @global_array, i32 0, i32 4
  %val4 = load i32, ptr %ptr4
  
  %sum1 = add i32 %val0, %val1
  %sum2 = add i32 %sum1, %val2
  %sum3 = add i32 %sum2, %val3
  %sum4 = add i32 %sum3, %val4
  
  ret i32 %sum4
}

define i32 @main() {
entry:
  ; Test reading global constant
  %const_val = call i32 @test_global_read()
  %cmp1 = icmp eq i32 %const_val, 42
  br i1 %cmp1, label %test2, label %fail

test2:
  ; Test writing to global
  call void @test_global_write(i32 100)
  %counter1 = load i32, ptr @global_counter
  %cmp2 = icmp eq i32 %counter1, 100
  br i1 %cmp2, label %test3, label %fail

test3:
  ; Test increment
  %new_val = call i32 @test_global_increment()
  %cmp3 = icmp eq i32 %new_val, 101
  br i1 %cmp3, label %test4, label %fail

test4:
  ; Test array access (index 2 should be 30)
  %array_val = call i32 @test_global_array_access(i32 2)
  %cmp4 = icmp eq i32 %array_val, 30
  br i1 %cmp4, label %test5, label %fail

test5:
  ; Test array sum (10+20+30+40+50 = 150)
  %sum = call i32 @sum_global_array()
  %cmp5 = icmp eq i32 %sum, 150
  br i1 %cmp5, label %pass, label %fail

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