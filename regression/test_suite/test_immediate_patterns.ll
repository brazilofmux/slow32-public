; Test immediate value handling and constant patterns
; Focus on patterns that might trigger the shift-add bug seen in loops

define i32 @test_simple_add_immediate() {
entry:
  ; Test simple addition with immediates (like 48 + i)
  %val1 = add i32 48, 0   ; '0' character
  %val2 = add i32 48, 1   ; '1' character
  %val3 = add i32 48, 2   ; '2' character
  %val4 = add i32 48, 8   ; '8' character
  
  ; These should produce simple ADD instructions, not shift-add sequences
  %cmp1 = icmp eq i32 %val1, 48
  %cmp2 = icmp eq i32 %val2, 49
  %cmp3 = icmp eq i32 %val3, 50
  %cmp4 = icmp eq i32 %val4, 56
  
  %and1 = and i1 %cmp1, %cmp2
  %and2 = and i1 %and1, %cmp3
  %and3 = and i1 %and2, %cmp4
  
  br i1 %and3, label %pass, label %fail

pass:
  ret i32 1

fail:
  ret i32 0
}

define void @test_character_generation() {
entry:
  ; This mimics the pattern from feature-loops that fails
  %i = alloca i32, align 4
  store i32 0, ptr %i
  
  br label %loop

loop:
  %i_val = load i32, ptr %i
  %cmp = icmp slt i32 %i_val, 9
  br i1 %cmp, label %body, label %done

body:
  ; This is the problematic pattern: 48 + i
  ; Should generate ASCII '0' through '8'
  %ascii = add i32 48, %i_val
  call void @debug_char(i32 %ascii)
  
  ; Increment i
  %next = add i32 %i_val, 1
  store i32 %next, ptr %i
  br label %loop

done:
  ; Output newline
  call void @debug_char(i32 10)
  ret void
}

define i32 @test_immediate_loads() {
entry:
  ; Test loading various immediate values
  %small = or i32 0, 5           ; Small positive
  %byte = or i32 0, 255          ; Max byte
  %negative = or i32 0, -1       ; All ones
  %large = or i32 0, 65535       ; Max 16-bit
  
  ; Verify they loaded correctly
  %cmp1 = icmp eq i32 %small, 5
  %cmp2 = icmp eq i32 %byte, 255
  %cmp3 = icmp eq i32 %negative, -1
  %cmp4 = icmp eq i32 %large, 65535
  
  %and1 = and i1 %cmp1, %cmp2
  %and2 = and i1 %and1, %cmp3
  %and3 = and i1 %and2, %cmp4
  
  br i1 %and3, label %success, label %failure

success:
  ret i32 1

failure:
  ret i32 0
}

define i32 @test_constant_folding() {
entry:
  ; These should be constant folded, but test the patterns anyway
  %const1 = mul i32 2, 16        ; Could become shift
  %const2 = add i32 32, 32       ; Simple add
  %const3 = sub i32 100, 50      ; Simple sub
  %const4 = shl i32 1, 10        ; Shift to create 1024
  
  %cmp1 = icmp eq i32 %const1, 32
  %cmp2 = icmp eq i32 %const2, 64
  %cmp3 = icmp eq i32 %const3, 50
  %cmp4 = icmp eq i32 %const4, 1024
  
  %and1 = and i1 %cmp1, %cmp2
  %and2 = and i1 %and1, %cmp3
  %and3 = and i1 %and2, %cmp4
  
  br i1 %and3, label %pass, label %fail

pass:
  ret i32 1

fail:
  ret i32 0
}

define i32 @main() {
entry:
  ; Test simple immediate additions
  %result1 = call i32 @test_simple_add_immediate()
  %cmp1 = icmp eq i32 %result1, 1
  br i1 %cmp1, label %test2, label %fail

test2:
  ; Test immediate loads
  %result2 = call i32 @test_immediate_loads()
  %cmp2 = icmp eq i32 %result2, 1
  br i1 %cmp2, label %test3, label %fail

test3:
  ; Test constant folding patterns
  %result3 = call i32 @test_constant_folding()
  %cmp3 = icmp eq i32 %result3, 1
  br i1 %cmp3, label %test4, label %fail

test4:
  ; Test character generation (should output "012345678\n")
  call void @test_character_generation()
  ; If we get here without crashing, consider it a pass
  br label %pass

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