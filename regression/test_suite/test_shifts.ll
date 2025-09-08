; Test shift operations (both immediate and register operands)
; Tests SLL, SRL, SRA with various operand types

define i32 @test_shift_immediate() {
entry:
  ; Shift left by immediate
  %sll1 = shl i32 1, 4          ; 1 << 4 = 16
  %sll2 = shl i32 3, 2          ; 3 << 2 = 12
  %sum1 = add i32 %sll1, %sll2  ; 16 + 12 = 28
  
  ; Logical shift right by immediate
  %srl1 = lshr i32 64, 2        ; 64 >> 2 = 16
  %srl2 = lshr i32 128, 3       ; 128 >> 3 = 16
  %sum2 = add i32 %srl1, %srl2  ; 16 + 16 = 32
  
  ; Arithmetic shift right by immediate (with sign extension)
  %sra1 = ashr i32 -64, 2       ; -64 >>> 2 = -16
  %sra2 = ashr i32 32, 3        ; 32 >>> 3 = 4
  %sum3 = sub i32 %sra2, %sra1  ; 4 - (-16) = 20
  
  ; Combine all results
  %temp = add i32 %sum1, %sum2  ; 28 + 32 = 60
  %result = add i32 %temp, %sum3 ; 60 + 20 = 80
  
  ret i32 %result
}

define i32 @test_shift_register(i32 %shift_amount) {
entry:
  ; Shift left by register
  %val1 = shl i32 2, %shift_amount    ; 2 << shift_amount
  
  ; Logical shift right by register  
  %val2 = lshr i32 32, %shift_amount  ; 32 >> shift_amount
  
  ; Arithmetic shift right by register
  %val3 = ashr i32 -32, %shift_amount ; -32 >>> shift_amount
  
  ; For shift_amount = 2:
  ; val1 = 2 << 2 = 8
  ; val2 = 32 >> 2 = 8
  ; val3 = -32 >>> 2 = -8
  ; sum = 8 + 8 + (-8) = 8
  
  %temp = add i32 %val1, %val2
  %result = add i32 %temp, %val3
  
  ret i32 %result
}

define i32 @main() {
entry:
  ; Test immediate shifts
  %result1 = call i32 @test_shift_immediate()
  %cmp1 = icmp eq i32 %result1, 80
  br i1 %cmp1, label %test2, label %fail

test2:
  ; Test register shifts with shift_amount = 2
  %result2 = call i32 @test_shift_register(i32 2)
  %cmp2 = icmp eq i32 %result2, 8
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