# builtins64.s -- 64-bit integer helper functions for SLOW-32
#
# All functions follow the SLOW-32 ABI:
#   Args in r3-r10 (lo/hi pairs), return in r1:r2 (lo:hi)
#   Callee-saved: r11-r28
#
# Functions:
#   __muldi3    - 64-bit multiply
#   __ashldi3   - 64-bit left shift
#   __lshrdi3   - 64-bit logical right shift
#   __ashrdi3   - 64-bit arithmetic right shift
#   __udivmoddi3 - unsigned 64-bit divmod (internal)
#   __udivdi3   - unsigned 64-bit divide
#   __umoddi3   - unsigned 64-bit modulo
#   __divdi3    - signed 64-bit divide
#   __moddi3    - signed 64-bit modulo

.text

# ================================================================
# __muldi3: 64-bit multiply
# Args: r3:r4 (a_lo:a_hi), r5:r6 (b_lo:b_hi)
# Returns: r1:r2 (result_lo:result_hi)
#
# result_lo = a_lo * b_lo  (low 32 bits)
# result_hi = mulhu(a_lo, b_lo) + a_lo*b_hi + a_hi*b_lo
# ================================================================
.global __muldi3
__muldi3:
    mul  r1, r3, r5       # r1 = lo(a_lo * b_lo)
    mulhu r2, r3, r5      # r2 = hi(a_lo * b_lo)
    mul  r7, r3, r6       # r7 = lo(a_lo * b_hi)
    add  r2, r2, r7       # r2 += a_lo * b_hi
    mul  r7, r4, r5       # r7 = lo(a_hi * b_lo)
    add  r2, r2, r7       # r2 += a_hi * b_lo
    jalr r0, r31, 0

# ================================================================
# __ashldi3: 64-bit left shift
# Args: r3:r4 (val_lo:val_hi), r5 (shift amount)
# Returns: r1:r2 (result_lo:result_hi)
# ================================================================
.global __ashldi3
__ashldi3:
    addi r7, r0, 32
    slt  r8, r5, r7       # r8 = (shift < 32)
    beq  r8, r0, .Lashl_ge32
    # shift < 32
    beq  r5, r0, .Lashl_zero
    sll  r1, r3, r5       # lo = val_lo << shift
    sll  r2, r4, r5       # hi = val_hi << shift
    sub  r7, r7, r5       # r7 = 32 - shift
    srl  r7, r3, r7       # r7 = val_lo >> (32 - shift)
    or   r2, r2, r7       # hi |= carry from lo
    jalr r0, r31, 0
.Lashl_zero:
    addi r1, r3, 0
    addi r2, r4, 0
    jalr r0, r31, 0
.Lashl_ge32:
    # shift >= 32
    addi r1, r0, 0        # lo = 0
    sub  r7, r5, r7       # r7 = shift - 32
    sll  r2, r3, r7       # hi = val_lo << (shift - 32)
    jalr r0, r31, 0

# ================================================================
# __lshrdi3: 64-bit logical right shift
# Args: r3:r4 (val_lo:val_hi), r5 (shift amount)
# Returns: r1:r2 (result_lo:result_hi)
# ================================================================
.global __lshrdi3
__lshrdi3:
    addi r7, r0, 32
    slt  r8, r5, r7       # r8 = (shift < 32)
    beq  r8, r0, .Llshr_ge32
    # shift < 32
    beq  r5, r0, .Llshr_zero
    srl  r1, r3, r5       # lo = val_lo >> shift
    srl  r2, r4, r5       # hi = val_hi >> shift
    sub  r7, r7, r5       # r7 = 32 - shift
    sll  r7, r4, r7       # r7 = val_hi << (32 - shift)
    or   r1, r1, r7       # lo |= bits shifted down from hi
    jalr r0, r31, 0
.Llshr_zero:
    addi r1, r3, 0
    addi r2, r4, 0
    jalr r0, r31, 0
.Llshr_ge32:
    # shift >= 32
    addi r2, r0, 0        # hi = 0
    sub  r7, r5, r7       # r7 = shift - 32
    srl  r1, r4, r7       # lo = val_hi >> (shift - 32)
    jalr r0, r31, 0

# ================================================================
# __ashrdi3: 64-bit arithmetic right shift
# Args: r3:r4 (val_lo:val_hi), r5 (shift amount)
# Returns: r1:r2 (result_lo:result_hi)
# ================================================================
.global __ashrdi3
__ashrdi3:
    addi r7, r0, 32
    slt  r8, r5, r7       # r8 = (shift < 32)
    beq  r8, r0, .Lashr_ge32
    # shift < 32
    beq  r5, r0, .Lashr_zero
    srl  r1, r3, r5       # lo = val_lo >> shift (logical)
    sra  r2, r4, r5       # hi = val_hi >> shift (arithmetic)
    sub  r7, r7, r5       # r7 = 32 - shift
    sll  r7, r4, r7       # r7 = val_hi << (32 - shift)
    or   r1, r1, r7       # lo |= bits shifted down from hi
    jalr r0, r31, 0
.Lashr_zero:
    addi r1, r3, 0
    addi r2, r4, 0
    jalr r0, r31, 0
.Lashr_ge32:
    # shift >= 32
    srai r2, r4, 31       # hi = sign extension (all 0s or all 1s)
    sub  r7, r5, r7       # r7 = shift - 32
    sra  r1, r4, r7       # lo = val_hi >> (shift - 32) (arithmetic)
    jalr r0, r31, 0

# ================================================================
# __udivmoddi3: unsigned 64-bit divmod (internal helper)
# Args: r3:r4 (num_lo:num_hi), r5:r6 (den_lo:den_hi)
# Returns: r1:r2 (quotient_lo:quotient_hi)
#          r11:r12 (remainder_lo:remainder_hi)
# Uses callee-saved r11-r18
#
# Binary long division: 64 iterations
# ================================================================
__udivmoddi3:
    # Save callee-saved registers
    addi r29, r29, -32
    stw  r29, r11, 0
    stw  r29, r12, 4
    stw  r29, r13, 8
    stw  r29, r14, 12
    stw  r29, r15, 16
    stw  r29, r16, 20
    stw  r29, r17, 24
    stw  r29, r18, 28

    # ------------- fast paths mirrored from runtime/builtins.c -------------
    # udivmoddi3_core() has taken these since it was written and this file
    # never did, so every 64-bit divide on a host without LLVM paid 64
    # shift-subtract rounds even though the archive behind us held the fast
    # copy (GitHub #30).  Anything not caught here -- a divisor of 2^32 or
    # more, and division by zero, whose ~0 quotient the loop produces --
    # falls through to that loop, which is left exactly as it was.

    # num < den: quotient 0, remainder num.  The loop's own answer, 64
    # rounds sooner; checked first, as udivmoddi3_core() checks it.
    sltu r17, r4, r6
    bne  r17, r0, .Ludm_lt
    bne  r4, r6, .Ludm_ge
    sltu r17, r3, r5
    beq  r17, r0, .Ludm_ge
.Ludm_lt:
    addi r1, r0, 0
    addi r2, r0, 0
    addi r11, r3, 0
    addi r12, r4, 0
    jal  r0, .Ludm_fret
.Ludm_ge:
    # The divisor fits 32 bits: every power of ten to 10^9, so every
    # decimal rescale and every mag_to_digits() step.
    bne  r6, r0, .Ludm_slow
    beq  r5, r0, .Ludm_slow
    bne  r4, r0, .Ludm_f64

    # num < 2^32 too: a single unsigned 32/32 divide.
    addi r2, r0, 0
    addi r12, r0, 0
    or   r17, r3, r5
    blt  r17, r0, .Lf32_big
    div  r1, r3, r5
    rem  r11, r3, r5
    jal  r0, .Ludm_fret
.Lf32_big:
    blt  r5, r0, .Lf32_bigd
    # dividend >= 2^31, divisor < 2^31: halve, divide, double, correct once
    srli r17, r3, 1
    div  r17, r17, r5
    slli r1, r17, 1
    mul  r18, r1, r5
    sub  r11, r3, r18
    bltu r11, r5, .Ludm_fret
    addi r1, r1, 1
    sub  r11, r11, r5
    jal  r0, .Ludm_fret
.Lf32_bigd:
    # divisor >= 2^31: the quotient is 0 or 1
    sltu r1, r3, r5
    xori r1, r1, 1
    mul  r18, r1, r5
    sub  r11, r3, r18
    jal  r0, .Ludm_fret

.Ludm_f64:
    # q_hi = num_hi / den, r11 = num_hi % den  (so r11 < den, as divlu2 wants)
    or   r17, r4, r5
    blt  r17, r0, .Lfq_big
    div  r2, r4, r5
    rem  r11, r4, r5
    jal  r0, .Ludm_divlu
.Lfq_big:
    blt  r5, r0, .Lfq_bigd
    srli r17, r4, 1
    div  r17, r17, r5
    slli r2, r17, 1
    mul  r18, r2, r5
    sub  r11, r4, r18
    bltu r11, r5, .Ludm_divlu
    addi r2, r2, 1
    sub  r11, r11, r5
    jal  r0, .Ludm_divlu
.Lfq_bigd:
    sltu r2, r4, r5
    xori r2, r2, 1
    mul  r18, r2, r5
    sub  r11, r4, r18

.Ludm_divlu:
    # (r11:r3) / r5 with r11 < r5 -- Hacker's Delight divlu2 in base 2^16,
    # the same two-divisions-and-a-correction shape as runtime/builtins.c's
    # divlu32().  All arithmetic wraps modulo 2^32, as the algorithm intends.
    #   r6 = s   r7 = vn1  r8 = vn0  r9 = un32  r10 = un1  r13 = un0
    #   r14 = q1  r15 = rhat  r16 = un21  r17,r18 = temps
    # s = nlz32(v)
    addi r6, r0, 0
    addi r17, r5, 0
    srli r18, r17, 16
    bne  r18, r0, .Lnlz1
    addi r6, r6, 16
    slli r17, r17, 16
.Lnlz1:
    srli r18, r17, 24
    bne  r18, r0, .Lnlz2
    addi r6, r6, 8
    slli r17, r17, 8
.Lnlz2:
    srli r18, r17, 28
    bne  r18, r0, .Lnlz3
    addi r6, r6, 4
    slli r17, r17, 4
.Lnlz3:
    srli r18, r17, 30
    bne  r18, r0, .Lnlz4
    addi r6, r6, 2
    slli r17, r17, 2
.Lnlz4:
    blt  r17, r0, .Lnlz5
    addi r6, r6, 1
.Lnlz5:
    # v <<= s; vn1 = v >> 16; vn0 = v & 0xFFFF
    sll  r5, r5, r6
    srli r7, r5, 16
    slli r8, r5, 16
    srli r8, r8, 16
    # un32 = s ? (u1 << s) | (u0 >> (32 - s)) : u1
    beq  r6, r0, .Lun32_s0
    sll  r9, r11, r6
    addi r18, r0, 32
    sub  r18, r18, r6
    srl  r18, r3, r18
    or   r9, r9, r18
    jal  r0, .Lun32_done
.Lun32_s0:
    addi r9, r11, 0
.Lun32_done:
    # un10 = u0 << s; un1 = un10 >> 16; un0 = un10 & 0xFFFF
    sll  r17, r3, r6
    srli r10, r17, 16
    slli r13, r17, 16
    srli r13, r13, 16

    # --- first base-2^16 digit: q1 = un32 / vn1, rhat = un32 % vn1 ---
    blt  r9, r0, .Lq1_big
    div  r14, r9, r7
    rem  r15, r9, r7
    jal  r0, .Lq1_fix
.Lq1_big:
    srli r17, r9, 1
    div  r17, r17, r7
    slli r14, r17, 1
    mul  r18, r14, r7
    sub  r15, r9, r18
    bltu r15, r7, .Lq1_fix
    addi r14, r14, 1
    sub  r15, r15, r7
.Lq1_fix:
    # while (q1 >= b || q1*vn0 > b*rhat + un1) { q1--; rhat += vn1;
    #                                            if (rhat >= b) break; }
    srli r17, r14, 16
    bne  r17, r0, .Lq1_dec
    mul  r17, r14, r8
    slli r18, r15, 16
    add  r18, r18, r10
    sltu r18, r18, r17
    beq  r18, r0, .Lq1_done
.Lq1_dec:
    addi r14, r14, -1
    add  r15, r15, r7
    srli r17, r15, 16
    beq  r17, r0, .Lq1_fix
.Lq1_done:
    # un21 = un32*b + un1 - q1*v
    slli r16, r9, 16
    add  r16, r16, r10
    mul  r17, r14, r5
    sub  r16, r16, r17

    # --- second digit: q0 = un21 / vn1, rhat = un21 % vn1 ---
    blt  r16, r0, .Lq0_big
    div  r1, r16, r7
    rem  r15, r16, r7
    jal  r0, .Lq0_fix
.Lq0_big:
    srli r17, r16, 1
    div  r17, r17, r7
    slli r1, r17, 1
    mul  r18, r1, r7
    sub  r15, r16, r18
    bltu r15, r7, .Lq0_fix
    addi r1, r1, 1
    sub  r15, r15, r7
.Lq0_fix:
    srli r17, r1, 16
    bne  r17, r0, .Lq0_dec
    mul  r17, r1, r8
    slli r18, r15, 16
    add  r18, r18, r13
    sltu r18, r18, r17
    beq  r18, r0, .Lq0_done
.Lq0_dec:
    addi r1, r1, -1
    add  r15, r15, r7
    srli r17, r15, 16
    beq  r17, r0, .Lq0_fix
.Lq0_done:
    # rem = (un21*b + un0 - q0*v) >> s ; quotient_lo = q1*b + q0
    slli r17, r16, 16
    add  r17, r17, r13
    mul  r18, r1, r5
    sub  r17, r17, r18
    srl  r11, r17, r6
    slli r18, r14, 16
    add  r1, r1, r18
    addi r12, r0, 0

.Ludm_fret:
    ldw  r13, r29, 8
    ldw  r14, r29, 12
    ldw  r15, r29, 16
    ldw  r16, r29, 20
    ldw  r17, r29, 24
    ldw  r18, r29, 28
    addi r29, r29, 32
    jalr r0, r31, 0

.Ludm_slow:

    # r11:r12 = remainder (starts at 0)
    addi r11, r0, 0
    addi r12, r0, 0
    # r13:r14 = quotient (starts at 0)
    addi r13, r0, 0
    addi r14, r0, 0
    # r15:r16 = numerator (r3:r4)
    addi r15, r3, 0
    addi r16, r4, 0
    # r17 = bit counter (63 downto 0)
    addi r17, r0, 63

.Ludm_loop:
    # remainder = remainder << 1
    srli r18, r11, 31     # r18 = top bit of rem_lo
    slli r12, r12, 1      # rem_hi <<= 1
    or   r12, r12, r18    # rem_hi |= carry from lo
    slli r11, r11, 1      # rem_lo <<= 1

    # Extract bit [r17] from numerator (r15:r16)
    # If r17 >= 32, bit is in r16 at position r17-32
    # If r17 < 32, bit is in r15 at position r17
    addi r7, r0, 32
    slt  r8, r17, r7
    beq  r8, r0, .Ludm_hi_bit
    # bit from r15
    srl  r18, r15, r17
    jal  r0, .Ludm_got_bit
.Ludm_hi_bit:
    sub  r7, r17, r7      # r7 = r17 - 32
    srl  r18, r16, r7
.Ludm_got_bit:
    andi r18, r18, 1
    or   r11, r11, r18    # remainder |= extracted bit

    # Compare remainder >= denominator
    # (r11:r12) >= (r5:r6)
    # Check hi first
    sltu r7, r6, r12       # r7 = (den_hi < rem_hi) => rem > den
    bne  r7, r0, .Ludm_sub
    sne  r7, r12, r6       # if hi parts differ and rem_hi < den_hi
    bne  r7, r0, .Ludm_next
    # hi parts equal, check lo
    sltu r7, r11, r5       # r7 = (rem_lo < den_lo)
    bne  r7, r0, .Ludm_next
    # rem >= den, fall through to sub

.Ludm_sub:
    # remainder -= denominator
    sltu r8, r11, r5       # borrow from lo
    sub  r11, r11, r5
    sub  r12, r12, r6
    sub  r12, r12, r8

    # Set bit [r17] in quotient
    addi r7, r0, 32
    slt  r8, r17, r7
    beq  r8, r0, .Ludm_set_hi_bit
    # set bit in r13 (lo word)
    addi r7, r0, 1
    sll  r7, r7, r17
    or   r13, r13, r7
    jal  r0, .Ludm_next
.Ludm_set_hi_bit:
    addi r8, r0, 32
    sub  r7, r17, r8
    addi r8, r0, 1
    sll  r8, r8, r7
    or   r14, r14, r8

.Ludm_next:
    beq  r17, r0, .Ludm_done
    addi r17, r17, -1
    jal  r0, .Ludm_loop

.Ludm_done:
    # Return quotient in r1:r2, remainder stays in r11:r12
    addi r1, r13, 0
    addi r2, r14, 0

    # Restore callee-saved (except r11:r12 which hold remainder)
    ldw  r13, r29, 8
    ldw  r14, r29, 12
    ldw  r15, r29, 16
    ldw  r16, r29, 20
    ldw  r17, r29, 24
    ldw  r18, r29, 28
    # r11, r12 restored by caller if needed
    addi r29, r29, 32
    jalr r0, r31, 0

# ================================================================
# __udivdi3: unsigned 64-bit divide
# Args: r3:r4 / r5:r6
# Returns: r1:r2 (quotient)
# ================================================================
.global __udivdi3
__udivdi3:
    addi r29, r29, -12
    stw  r29, r31, 0
    stw  r29, r11, 4
    stw  r29, r12, 8
    jal  r31, __udivmoddi3
    # r1:r2 already has quotient
    ldw  r11, r29, 4
    ldw  r12, r29, 8
    ldw  r31, r29, 0
    addi r29, r29, 12
    jalr r0, r31, 0

# ================================================================
# __umoddi3: unsigned 64-bit modulo
# Args: r3:r4 % r5:r6
# Returns: r1:r2 (remainder)
# ================================================================
.global __umoddi3
__umoddi3:
    addi r29, r29, -12
    stw  r29, r31, 0
    stw  r29, r11, 4
    stw  r29, r12, 8
    jal  r31, __udivmoddi3
    # remainder is in r11:r12
    addi r1, r11, 0
    addi r2, r12, 0
    ldw  r11, r29, 4
    ldw  r12, r29, 8
    ldw  r31, r29, 0
    addi r29, r29, 12
    jalr r0, r31, 0

# ================================================================
# __divdi3: signed 64-bit divide
# Args: r3:r4 / r5:r6 (signed)
# Returns: r1:r2 (quotient)
#
# Strategy: negate negative inputs, do unsigned divide,
# negate result if signs differed
# ================================================================
.global __divdi3
__divdi3:
    addi r29, r29, -20
    stw  r29, r31, 0
    stw  r29, r11, 4
    stw  r29, r12, 8
    stw  r29, r13, 12
    stw  r29, r14, 16

    # r13 = sign flag (1 if result should be negative)
    addi r13, r0, 0

    # Check if numerator is negative (r4 < 0)
    slt  r7, r4, r0
    beq  r7, r0, .Ldiv_num_pos
    # Negate r3:r4
    addi r13, r0, 1
    sltu r8, r0, r3       # borrow = (0 < r3)
    sub  r3, r0, r3
    sub  r4, r0, r4
    sub  r4, r4, r8
.Ldiv_num_pos:

    # Check if denominator is negative (r6 < 0)
    slt  r7, r6, r0
    beq  r7, r0, .Ldiv_den_pos
    # Negate r5:r6
    addi r7, r0, 1
    sub  r13, r7, r13      # flip sign flag
    sltu r8, r0, r5
    sub  r5, r0, r5
    sub  r6, r0, r6
    sub  r6, r6, r8
.Ldiv_den_pos:

    jal  r31, __udivmoddi3

    # Negate result if sign flag set
    beq  r13, r0, .Ldiv_done
    sltu r8, r0, r1
    sub  r1, r0, r1
    sub  r2, r0, r2
    sub  r2, r2, r8

.Ldiv_done:
    ldw  r11, r29, 4
    ldw  r12, r29, 8
    ldw  r13, r29, 12
    ldw  r14, r29, 16
    ldw  r31, r29, 0
    addi r29, r29, 20
    jalr r0, r31, 0

# ================================================================
# __moddi3: signed 64-bit modulo
# Args: r3:r4 % r5:r6 (signed)
# Returns: r1:r2 (remainder, sign follows numerator)
# ================================================================
.global __moddi3
__moddi3:
    addi r29, r29, -20
    stw  r29, r31, 0
    stw  r29, r11, 4
    stw  r29, r12, 8
    stw  r29, r13, 12
    stw  r29, r14, 16

    # r13 = sign of numerator (1 if negative)
    addi r13, r0, 0

    # Check if numerator is negative
    slt  r7, r4, r0
    beq  r7, r0, .Lmod_num_pos
    addi r13, r0, 1
    sltu r8, r0, r3
    sub  r3, r0, r3
    sub  r4, r0, r4
    sub  r4, r4, r8
.Lmod_num_pos:

    # Check if denominator is negative
    slt  r7, r6, r0
    beq  r7, r0, .Lmod_den_pos
    sltu r8, r0, r5
    sub  r5, r0, r5
    sub  r6, r0, r6
    sub  r6, r6, r8
.Lmod_den_pos:

    jal  r31, __udivmoddi3

    # remainder is in r11:r12
    addi r1, r11, 0
    addi r2, r12, 0

    # Negate remainder if numerator was negative
    beq  r13, r0, .Lmod_done
    sltu r8, r0, r1
    sub  r1, r0, r1
    sub  r2, r0, r2
    sub  r2, r2, r8

.Lmod_done:
    ldw  r11, r29, 4
    ldw  r12, r29, 8
    ldw  r13, r29, 12
    ldw  r14, r29, 16
    ldw  r31, r29, 0
    addi r29, r29, 20
    jalr r0, r31, 0

# ---- Unsigned 32-bit divide/modulo (bootstrap copies; libs32 has
# ---- its own for app links).  __umodsi3 = n - (n/d)*d.
    .global __udivsi3
    .global __divsi3

# Unsigned 32-bit division on the hardware signed divider (the same
# routine as runtime/divsi3.s; see the notes there).
# r3 = dividend, r4 = divisor, result in r1
__udivsi3:
    beq r4, r0, .udiv_divzero
    or r5, r3, r4
    blt r5, r0, .udiv_big
    div r1, r3, r4
    jalr r0, r31, 0

.udiv_big:
    blt r4, r0, .udiv_bigdiv
    srli r5, r3, 1
    div r5, r5, r4
    slli r1, r5, 1
    mul r6, r1, r4
    sub r6, r3, r6
    bltu r6, r4, .udiv_done
    addi r1, r1, 1
.udiv_done:
    jalr r0, r31, 0

.udiv_bigdiv:
    sltu r1, r3, r4
    xori r1, r1, 1
    jalr r0, r31, 0

.udiv_divzero:
    addi r1, r0, -1
    jalr r0, r31, 0

# Signed 32-bit division: the hardware divider; a zero divisor gives -1.
# r3 = dividend, r4 = divisor, result in r1
__divsi3:
    beq r4, r0, .div_divzero
    div r1, r3, r4
    jalr r0, r31, 0

.div_divzero:
    addi r1, r0, -1
    jalr r0, r31, 0

.global __umodsi3
__umodsi3:
    beq r4, r0, .umod_zero
    or r5, r3, r4
    blt r5, r0, .umod_slow
    rem r1, r3, r4
    jalr r0, r31, 0
.umod_zero:
    addi r1, r3, 0
    jalr r0, r31, 0
.umod_slow:
    addi r29, r29, -16
    stw r29, r31, 12
    stw r29, r11, 8
    stw r29, r12, 4
    addi r11, r3, 0
    addi r12, r4, 0
    jal r31, __udivsi3
    mul r1, r1, r12
    sub r1, r11, r1
    ldw r31, r29, 12
    ldw r11, r29, 8
    ldw r12, r29, 4
    addi r29, r29, 16
    jalr r0, r31, 0
