; Test edge cases and corner conditions
; These tests validate boundary conditions and special values

define i32 @test_zero_operations() {
entry:
  ; Operations with zero
  %add_zero = add i32 0, 0          ; 0 + 0 = 0
  %mul_zero = mul i32 100, 0        ; 100 * 0 = 0
  %div_by_one = sdiv i32 42, 1      ; 42 / 1 = 42
  %rem_one = srem i32 100, 1        ; 100 % 1 = 0
  
  ; Combine results (0 + 0 + 42 + 0 = 42)
  %sum1 = add i32 %add_zero, %mul_zero
  %sum2 = add i32 %sum1, %div_by_one
  %result = add i32 %sum2, %rem_one
  
  ret i32 %result  ; Should be 42
}

define i32 @test_negative_numbers() {
entry:
  ; Operations with negative numbers
  %neg1 = sub i32 0, 10             ; -10
  %neg2 = sub i32 0, 20             ; -20
  %sum_neg = add i32 %neg1, %neg2   ; -30
  %mul_neg = mul i32 %neg1, %neg2   ; (-10) * (-20) = 200
  %div_neg = sdiv i32 %mul_neg, %neg1 ; 200 / (-10) = -20
  
  ; -30 + (-20) = -50
  %result = add i32 %sum_neg, %div_neg
  
  ; Return absolute value for easier testing
  %is_neg = icmp slt i32 %result, 0
  br i1 %is_neg, label %negate, label %done

negate:
  %abs = sub i32 0, %result
  ret i32 %abs  ; Should be 50

done:
  ret i32 %result
}

define i32 @test_max_min_values() {
entry:
  ; Test with maximum positive value (2^31 - 1 = 2147483647)
  %max = or i32 2147483647, 0
  
  ; Test with minimum negative value (-2^31 = -2147483648)
  %min = or i32 -2147483648, 0
  
  ; Adding 1 to max should overflow to min
  %overflow = add i32 %max, 1
  %is_min = icmp eq i32 %overflow, %min
  
  ; Subtracting 1 from min should underflow to max  
  %underflow = sub i32 %min, 1
  %is_max = icmp eq i32 %underflow, %max
  
  ; Both conditions should be true
  %both = and i1 %is_min, %is_max
  br i1 %both, label %pass, label %fail

pass:
  ret i32 1

fail:
  ret i32 0
}

define i32 @test_shift_edge_cases() {
entry:
  ; Shift by 0 (should be identity)
  %shl0 = shl i32 42, 0         ; 42 << 0 = 42
  %shr0 = lshr i32 42, 0        ; 42 >> 0 = 42
  
  ; Shift by 31 (max valid shift)
  %shl31 = shl i32 1, 31        ; 1 << 31 = 0x80000000
  %shr31 = lshr i32 %shl31, 31 ; 0x80000000 >> 31 = 1
  
  ; Test results
  %cmp1 = icmp eq i32 %shl0, 42
  %cmp2 = icmp eq i32 %shr0, 42
  %cmp3 = icmp eq i32 %shr31, 1
  
  %and1 = and i1 %cmp1, %cmp2
  %and2 = and i1 %and1, %cmp3
  
  br i1 %and2, label %success, label %failure

success:
  ret i32 1

failure:
  ret i32 0
}

define i32 @test_comparison_edge_cases() {
entry:
  ; Compare with self
  %self_eq = icmp eq i32 42, 42       ; true
  %self_ne = icmp ne i32 42, 42       ; false
  %self_lt = icmp slt i32 42, 42      ; false
  %self_le = icmp sle i32 42, 42      ; true
  
  ; Compare zero with positive/negative
  %zero_pos = icmp slt i32 0, 1       ; true
  %zero_neg = icmp sgt i32 0, -1      ; true
  
  ; All expected conditions
  %cond1 = and i1 %self_eq, %self_le
  %cond2 = and i1 %zero_pos, %zero_neg
  %all_conds = and i1 %cond1, %cond2
  
  ; Check that self_ne and self_lt are false
  %not_ne = xor i1 %self_ne, 1
  %not_lt = xor i1 %self_lt, 1
  %negatives = and i1 %not_ne, %not_lt
  
  %final = and i1 %all_conds, %negatives
  br i1 %final, label %pass, label %fail

pass:
  ret i32 1

fail:
  ret i32 0
}

define i32 @main() {
entry:
  ; Test zero operations
  %result1 = call i32 @test_zero_operations()
  %cmp1 = icmp eq i32 %result1, 42
  br i1 %cmp1, label %test2, label %fail

test2:
  ; Test negative numbers
  %result2 = call i32 @test_negative_numbers()
  %cmp2 = icmp eq i32 %result2, 50
  br i1 %cmp2, label %test3, label %fail

test3:
  ; Test overflow/underflow
  %result3 = call i32 @test_max_min_values()
  %cmp3 = icmp eq i32 %result3, 1
  br i1 %cmp3, label %test4, label %fail

test4:
  ; Test shift edge cases
  %result4 = call i32 @test_shift_edge_cases()
  %cmp4 = icmp eq i32 %result4, 1
  br i1 %cmp4, label %test5, label %fail

test5:
  ; Test comparison edge cases
  %result5 = call i32 @test_comparison_edge_cases()
  %cmp5 = icmp eq i32 %result5, 1
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