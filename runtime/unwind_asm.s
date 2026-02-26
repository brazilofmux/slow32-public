# SLOW-32 Unwind Assembly Glue
#
# Provides the entry points for _Unwind_RaiseException and _Unwind_Resume,
# which save the caller's register state into a _Unwind_Context on the stack,
# call the C implementation, and (on success) restore context to jump to
# a landing pad.
#
# _Unwind_Context layout (144 bytes):
#   offset 0:   regs[0..31] — 32 x 4 bytes = 128 bytes
#   offset 128: ip           — 4 bytes
#   offset 132: region_start — 4 bytes (filled by C code)
#   offset 136: lsda         — 4 bytes (filled by C code)
#   offset 140: personality  — 4 bytes (filled by C code)
#   Total: 144 bytes
#
# STW syntax: stw base+offset, value  (stores value at base+offset)
# LDW syntax: ldw dest, base, offset  (loads dest from base+offset)

.global _Unwind_RaiseException
.global _Unwind_Resume
.global _unwind_restore_context

# ========================================================================
# _Unwind_RaiseException(struct _Unwind_Exception *exc)
#
# r3 = exc pointer (arg 1, preserved for C call)
# Saves caller's registers into a stack-allocated _Unwind_Context,
# then calls _Unwind_RaiseException_impl(exc, ctx).
# If the C code returns (no handler found), we return the error code.
# ========================================================================
_Unwind_RaiseException:
    # Save our return address and frame pointer
    addi sp, sp, -160            # 144 (context) + 8 (saved fp/lr) + 8 (alignment)
    stw  sp+152, fp              # save old fp
    stw  sp+156, lr              # save old lr
    addi fp, sp, 0               # new frame pointer

    # Context is at sp+0 (144 bytes)
    # Save all general registers as they were in the CALLER's frame.
    # r0 is always 0
    stw  sp+0,   r0              # regs[0] = 0
    stw  sp+4,   r1              # regs[1]
    stw  sp+8,   r2              # regs[2]
    stw  sp+12,  r3              # regs[3] (exc ptr)
    stw  sp+16,  r4              # regs[4]
    stw  sp+20,  r5              # regs[5]
    stw  sp+24,  r6              # regs[6]
    stw  sp+28,  r7              # regs[7]
    stw  sp+32,  r8              # regs[8]
    stw  sp+36,  r9              # regs[9]
    stw  sp+40,  r10             # regs[10]
    stw  sp+44,  r11             # regs[11]
    stw  sp+48,  r12             # regs[12]
    stw  sp+52,  r13             # regs[13]
    stw  sp+56,  r14             # regs[14]
    stw  sp+60,  r15             # regs[15]
    stw  sp+64,  r16             # regs[16]
    stw  sp+68,  r17             # regs[17]
    stw  sp+72,  r18             # regs[18]
    stw  sp+76,  r19             # regs[19]
    stw  sp+80,  r20             # regs[20]
    stw  sp+84,  r21             # regs[21]
    stw  sp+88,  r22             # regs[22]
    stw  sp+92,  r23             # regs[23]
    stw  sp+96,  r24             # regs[24]
    stw  sp+100, r25             # regs[25]
    stw  sp+104, r26             # regs[26]
    stw  sp+108, r27             # regs[27]
    stw  sp+112, r28             # regs[28]
    # For SP (r29), store the caller's SP (before our allocation)
    addi r1,  sp, 160
    stw  sp+116, r1              # regs[29] = caller's SP
    # For FP (r30), store the caller's saved FP
    ldw  r1,  sp, 152
    stw  sp+120, r1              # regs[30] = caller's FP
    # For LR (r31), store the caller's return address = our return address
    ldw  r1,  sp, 156
    stw  sp+124, r1              # regs[31] = caller's LR
    # IP = caller's return address (where to resume if no handler)
    stw  sp+128, r1              # ip = LR

    # Call _Unwind_RaiseException_impl(exc, ctx)
    # r3 = exc (already in r3)
    addi r4, sp, 0               # r4 = &context (on stack)
    jal  _Unwind_RaiseException_impl

    # If we return here, unwinding failed. Return code is in r1.
    # Restore saved registers and return error to caller.
    ldw  fp, sp, 152
    ldw  lr, sp, 156
    addi sp, sp, 160
    ret

# ========================================================================
# _Unwind_Resume(struct _Unwind_Exception *exc)
#
# Called from a cleanup landing pad to continue Phase 2 unwinding.
# r3 = exc pointer
# This function does not return — it either transfers to another landing pad
# via _unwind_restore_context or traps.
# ========================================================================
_Unwind_Resume:
    # Save registers into a context (same as RaiseException)
    addi sp, sp, -160
    stw  sp+152, fp
    stw  sp+156, lr
    addi fp, sp, 0

    # Save all registers into context at sp+0
    stw  sp+0,   r0
    stw  sp+4,   r1
    stw  sp+8,   r2
    stw  sp+12,  r3
    stw  sp+16,  r4
    stw  sp+20,  r5
    stw  sp+24,  r6
    stw  sp+28,  r7
    stw  sp+32,  r8
    stw  sp+36,  r9
    stw  sp+40,  r10
    stw  sp+44,  r11
    stw  sp+48,  r12
    stw  sp+52,  r13
    stw  sp+56,  r14
    stw  sp+60,  r15
    stw  sp+64,  r16
    stw  sp+68,  r17
    stw  sp+72,  r18
    stw  sp+76,  r19
    stw  sp+80,  r20
    stw  sp+84,  r21
    stw  sp+88,  r22
    stw  sp+92,  r23
    stw  sp+96,  r24
    stw  sp+100, r25
    stw  sp+104, r26
    stw  sp+108, r27
    stw  sp+112, r28
    addi r1,  sp, 160
    stw  sp+116, r1              # regs[29] = caller's SP
    ldw  r1,  sp, 152
    stw  sp+120, r1              # regs[30] = caller's FP
    ldw  r1,  sp, 156
    stw  sp+124, r1              # regs[31] = caller's LR
    stw  sp+128, r1              # ip = LR

    # Call _Unwind_Resume_impl(exc, ctx)
    # r3 = exc (already set)
    addi r4, sp, 0               # r4 = &context
    jal  _Unwind_Resume_impl

    # Should not return — trap if it does
    halt

# ========================================================================
# _unwind_restore_context(_Unwind_Context *ctx)
#
# Restores all registers from the context and jumps to ctx->ip.
# This is called at the end of Phase 2 to transfer control to the
# landing pad. Does not return.
# r3 = pointer to _Unwind_Context
# ========================================================================
_unwind_restore_context:
    # Load the target IP first (offset 128)
    ldw  r1, r3, 128             # r1 = target IP (landing pad)

    # Save target IP in r2 (scratch — not used by landing pads)
    add  r2, r1, r0              # r2 = target IP

    # Restore registers from context (skip r0=zero, r2=tempIP, r3=base for now)
    ldw  r4,  r3, 16
    ldw  r5,  r3, 20
    ldw  r6,  r3, 24
    ldw  r7,  r3, 28
    ldw  r8,  r3, 32
    ldw  r9,  r3, 36
    ldw  r10, r3, 40
    ldw  r11, r3, 44
    ldw  r12, r3, 48
    ldw  r13, r3, 52
    ldw  r14, r3, 56
    ldw  r15, r3, 60
    ldw  r16, r3, 64
    ldw  r17, r3, 68
    ldw  r18, r3, 72
    ldw  r19, r3, 76
    ldw  r20, r3, 80
    ldw  r21, r3, 84
    ldw  r22, r3, 88
    ldw  r23, r3, 92
    ldw  r24, r3, 96
    ldw  r25, r3, 100
    ldw  r26, r3, 104
    ldw  r27, r3, 108
    ldw  r28, r3, 112
    ldw  r29, r3, 116            # restore SP
    ldw  r30, r3, 120            # restore FP
    ldw  r31, r3, 124            # restore LR

    # Restore r1 from context
    ldw  r1,  r3, 4

    # Restore r3 last (destroys our base pointer)
    # The personality routine has set r3 = exception ptr and r4 = selector via SetGR
    ldw  r3,  r3, 12

    # Jump to landing pad (target IP is in r2)
    jalr r0, r2, 0
