# Additional runtime functions for printf support

# memcpy - alias to llvm.memcpy.p0.p0.i32
.global memcpy
memcpy:
    beq r0, r0, llvm.memcpy.p0.p0.i32

# strlen - simple implementation
.global strlen
strlen:
    add r11, r3, r0      # r11 = string pointer
    add r1, r0, r0       # r1 = length counter
.strlen_loop:
    ldbu r12, r11+0      # load byte
    beq r12, r0, .strlen_done
    addi r11, r11, 1     # advance pointer
    addi r1, r1, 1       # increment counter
    beq r0, r0, .strlen_loop
.strlen_done:
    jalr r0, lr, 0

# va_start/va_end - just nops for now
.global llvm.va_start.p0
llvm.va_start.p0:
    jalr r0, lr, 0

.global llvm.va_end.p0
llvm.va_end.p0:
    jalr r0, lr, 0