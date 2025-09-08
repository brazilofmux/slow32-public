; Test branch instructions and comparisons
; Tests BEQ, BNE, BLT, BGE and all comparison operations

define i32 @test_eq_ne(i32 %a, i32 %b) {
entry:
  %eq = icmp eq i32 %a, %b
  br i1 %eq, label %equal, label %not_equal

equal:
  ret i32 1

not_equal:
  ret i32 0
}

define i32 @test_signed_comparisons(i32 %a, i32 %b) {
entry:
  ; Test signed less than
  %slt = icmp slt i32 %a, %b
  br i1 %slt, label %a_less, label %check_greater

a_less:
  ret i32 -1

check_greater:
  ; Test signed greater than
  %sgt = icmp sgt i32 %a, %b
  br i1 %sgt, label %a_greater, label %a_equal

a_greater:
  ret i32 1

a_equal:
  ret i32 0
}

define i32 @test_unsigned_comparisons(i32 %a, i32 %b) {
entry:
  ; Test unsigned less than
  %ult = icmp ult i32 %a, %b
  br i1 %ult, label %a_less, label %check_greater

a_less:
  ret i32 -1

check_greater:
  ; Test unsigned greater than
  %ugt = icmp ugt i32 %a, %b
  br i1 %ugt, label %a_greater, label %a_equal

a_greater:
  ret i32 1

a_equal:
  ret i32 0
}

define i32 @test_complex_control_flow(i32 %x) {
entry:
  ; Test multiple branches and phi nodes
  %cmp1 = icmp slt i32 %x, 10
  br i1 %cmp1, label %small, label %check_medium

small:
  %small_val = mul i32 %x, 2
  br label %done

check_medium:
  %cmp2 = icmp slt i32 %x, 100
  br i1 %cmp2, label %medium, label %large

medium:
  %medium_val = add i32 %x, 50
  br label %done

large:
  %large_val = sdiv i32 %x, 2
  br label %done

done:
  %result = phi i32 [ %small_val, %small ], [ %medium_val, %medium ], [ %large_val, %large ]
  ret i32 %result
}

define i32 @test_loop_with_branches() {
entry:
  %sum = alloca i32, align 4
  store i32 0, ptr %sum
  
  %i = alloca i32, align 4
  store i32 1, ptr %i
  
  br label %loop_header

loop_header:
  %i_val = load i32, ptr %i
  %cmp = icmp sle i32 %i_val, 10
  br i1 %cmp, label %loop_body, label %loop_exit

loop_body:
  ; Add only even numbers
  %rem = srem i32 %i_val, 2
  %is_even = icmp eq i32 %rem, 0
  br i1 %is_even, label %add_to_sum, label %skip

add_to_sum:
  %old_sum = load i32, ptr %sum
  %new_sum = add i32 %old_sum, %i_val
  store i32 %new_sum, ptr %sum
  br label %skip

skip:
  %old_i = load i32, ptr %i
  %new_i = add i32 %old_i, 1
  store i32 %new_i, ptr %i
  br label %loop_header

loop_exit:
  %final_sum = load i32, ptr %sum
  ret i32 %final_sum  ; Should be 2+4+6+8+10 = 30
}

define i32 @main() {
entry:
  ; Test equality
  %result1 = call i32 @test_eq_ne(i32 42, i32 42)
  %cmp1 = icmp eq i32 %result1, 1
  br i1 %cmp1, label %test2, label %fail

test2:
  ; Test inequality
  %result2 = call i32 @test_eq_ne(i32 10, i32 20)
  %cmp2 = icmp eq i32 %result2, 0
  br i1 %cmp2, label %test3, label %fail

test3:
  ; Test signed comparisons (5 < 10)
  %result3 = call i32 @test_signed_comparisons(i32 5, i32 10)
  %cmp3 = icmp eq i32 %result3, -1
  br i1 %cmp3, label %test4, label %fail

test4:
  ; Test complex control flow (x=50 should go to medium branch and return 100)
  %result4 = call i32 @test_complex_control_flow(i32 50)
  %cmp4 = icmp eq i32 %result4, 100
  br i1 %cmp4, label %test5, label %fail

test5:
  ; Test loop with conditional branches (sum of even numbers 1-10)
  %result5 = call i32 @test_loop_with_branches()
  %cmp5 = icmp eq i32 %result5, 30
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