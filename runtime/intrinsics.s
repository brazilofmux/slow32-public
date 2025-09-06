# SLOW-32 Intrinsics Library
# Implementations of LLVM intrinsics that the compiler might emit

.global llvm.lifetime.start.p0
.global llvm.lifetime.end.p0
.global llvm.memcpy.p0.p0.i32
.global llvm.memcpy.p0.p0.i64
.global llvm.memset.p0.i32
.global llvm.memset.p0.i64
.global llvm.smax.i32
.global llvm.smin.i32
.global llvm.umax.i32
.global llvm.umax.i64
.global llvm.umin.i32
.global llvm.va_start.p0
.global llvm.va_end.p0

# Varargs support - implemented as no-ops since our ABI passes varargs on stack
llvm.va_start.p0:
    jalr r0, lr, 0  # va_start is a no-op - args already on stack

llvm.va_end.p0:
    jalr r0, lr, 0  # va_end is a no-op

# Lifetime intrinsics - just return immediately (nops)
# Args: r3 = size, r4 = ptr
llvm.lifetime.start.p0:
    jalr r0, lr, 0

llvm.lifetime.end.p0:
    jalr r0, lr, 0

# Memory copy (both i32 and i64 versions point here)
# Args: r3 = dest, r4 = src, r5 = size (bytes)
llvm.memcpy.p0.p0.i64:
llvm.memcpy.p0.p0.i32:
    addi sp, sp, -32
    stw sp+0, lr
    stw sp+4, fp
    add fp, sp, r0
    
    # Save registers we'll use
    stw sp+8, r11
    stw sp+12, r12
    stw sp+16, r13
    
    add r11, r3, r0   # dest
    add r12, r4, r0   # src  
    add r13, r5, r0   # count
    
.memcpy_loop:
    # while (count > 0) { ... }
    sle r14, r13, r0           # r14 = (count <= 0)
    beq r14, r0, .memcpy_body  # if !(count<=0) -> body
    beq r0, r0, .memcpy_done
.memcpy_body:
    ldw r15, r12+0
    stw r11+0, r15
    addi r11, r11, 4
    addi r12, r12, 4
    addi r13, r13, -4
    beq r0, r0, .memcpy_loop
.memcpy_done:
    # Restore registers
    ldw r11, sp+8
    ldw r12, sp+12
    ldw r13, sp+16
    
    # Return dest in r1
    add r1, r3, r0
    
    add sp, fp, r0
    ldw lr, sp+0
    ldw fp, sp+4
    addi sp, sp, 32
    jalr r0, lr, 0

# Memory set (both i32 and i64 versions point here)
# Args: r3 = dest, r4 = value (byte), r5 = size
llvm.memset.p0.i64:
llvm.memset.p0.i32:
    addi sp, sp, -32
    stw sp+0, lr
    stw sp+4, fp
    add fp, sp, r0
    
    # Save registers
    stw sp+8, r11
    stw sp+12, r12
    stw sp+16, r13
    
    add r11, r3, r0   # dest
    add r12, r4, r0   # value (we'll replicate to word)
    add r13, r5, r0   # count
    
    # Replicate byte to full word: 0x01 -> 0x01010101
    andi r12, r12, 0xFF
    sll r14, r12, 8
    or r12, r12, r14     # 0x0101
    sll r14, r12, 16
    or r12, r12, r14     # 0x01010101
    
.memset_loop:
    # while (count > 0) { ... }
    sle r14, r13, r0           # r14 = (count <= 0)
    beq r14, r0, .memset_body  # if !(count<=0) -> body
    beq r0, r0, .memset_done
.memset_body:
    stw r11+0, r12
    addi r11, r11, 4
    addi r13, r13, -4
    beq r0, r0, .memset_loop
.memset_done:
    # Restore registers
    ldw r11, sp+8
    ldw r12, sp+12
    ldw r13, sp+16
    
    # Return dest in r1
    add r1, r3, r0
    
    add sp, fp, r0
    ldw lr, sp+0
    ldw fp, sp+4
    addi sp, sp, 32
    jalr r0, lr, 0

# Signed maximum
# Args: r3 = a, r4 = b
# Returns: r1 = max(a, b)
llvm.smax.i32:
    sgt r11, r3, r4      # r11 = (a > b)
    beq r11, r0, .smax_b
    add r1, r3, r0       # return a
    jalr r0, lr, 0
.smax_b:
    add r1, r4, r0       # return b
    jalr r0, lr, 0

# Signed minimum
# Args: r3 = a, r4 = b
# Returns: r1 = min(a, b)
llvm.smin.i32:
    slt r11, r3, r4      # r11 = (a < b)
    beq r11, r0, .smin_b
    add r1, r3, r0       # return a
    jalr r0, lr, 0
.smin_b:
    add r1, r4, r0       # return b
    jalr r0, lr, 0

# Unsigned maximum (both i32 and i64 versions - we only support 32-bit)
# Args: r3 = a, r4 = b
# Returns: r1 = max(a, b)
llvm.umax.i64:
llvm.umax.i32:
    sgtu r11, r3, r4     # r11 = (a > b) unsigned
    beq r11, r0, .umax_b
    add r1, r3, r0       # return a
    jalr r0, lr, 0
.umax_b:
    add r1, r4, r0       # return b
    jalr r0, lr, 0

# Unsigned minimum
# Args: r3 = a, r4 = b
# Returns: r1 = min(a, b)
llvm.umin.i32:
    sltu r11, r3, r4     # r11 = (a < b) unsigned
    beq r11, r0, .umin_b
    add r1, r3, r0       # return a
    jalr r0, lr, 0
.umin_b:
    add r1, r4, r0       # return b
    jalr r0, lr, 0