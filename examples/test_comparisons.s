# Test all comparison instructions
# Tests both signed and unsigned comparisons

.global _start

.text
_start:
    # Initialize stack
    lui sp, 0x1000
    addi sp, sp, -16
    
    # Test values
    addi r10, r0, 5      # r10 = 5
    addi r11, r0, 10     # r11 = 10
    addi r12, r0, 5      # r12 = 5 (equal to r10)
    addi r13, r0, -5     # r13 = -5 (negative)
    
    # Test SLT (set less than, signed) - already existed
    slt r14, r10, r11    # r14 = 1 (5 < 10)
    slt r15, r11, r10    # r15 = 0 (10 < 5)
    slt r16, r13, r10    # r16 = 1 (-5 < 5)
    
    # Test SLTU (set less than, unsigned) - already existed  
    sltu r17, r10, r11   # r17 = 1 (5 < 10)
    sltu r18, r13, r10   # r18 = 0 (0xFFFFFFFB > 5 unsigned)
    
    # Test SEQ (set equal)
    seq r19, r10, r12    # r19 = 1 (5 == 5)
    seq r20, r10, r11    # r20 = 0 (5 == 10)
    
    # Test SNE (set not equal)
    sne r21, r10, r11    # r21 = 1 (5 != 10)
    sne r22, r10, r12    # r22 = 0 (5 != 5)
    
    # Test SGT (set greater than, signed)
    sgt r23, r11, r10    # r23 = 1 (10 > 5)
    sgt r24, r10, r11    # r24 = 0 (5 > 10)
    sgt r25, r10, r13    # r25 = 1 (5 > -5)
    
    # Test SGTU (set greater than, unsigned)
    sgtu r26, r11, r10   # r26 = 1 (10 > 5)
    sgtu r27, r13, r10   # r27 = 1 (0xFFFFFFFB > 5 unsigned)
    
    # Test SLE (set less or equal, signed)
    sle r1, r10, r11     # r1 = 1 (5 <= 10)
    sle r2, r10, r12     # r2 = 1 (5 <= 5)
    sle r3, r11, r10     # r3 = 0 (10 <= 5)
    
    # Test SLEU (set less or equal, unsigned)
    sleu r4, r10, r11    # r4 = 1 (5 <= 10)
    sleu r5, r13, r10    # r5 = 0 (0xFFFFFFFB <= 5 unsigned)
    
    # Test SGE (set greater or equal, signed)
    sge r6, r11, r10     # r6 = 1 (10 >= 5)
    sge r7, r10, r12     # r7 = 1 (5 >= 5)
    sge r8, r10, r11     # r8 = 0 (5 >= 10)
    
    # Test SGEU (set greater or equal, unsigned)
    sgeu r9, r11, r10    # r9 = 1 (10 >= 5)
    sgeu r28, r13, r10   # r28 = 1 (0xFFFFFFFB >= 5 unsigned)
    
    # Print results as ASCII '0' or '1'
    addi r29, r0, 48     # ASCII '0'
    
    # Print SLT results
    add r30, r14, r29
    debug r30            # Should print '1'
    add r30, r15, r29
    debug r30            # Should print '0'
    add r30, r16, r29
    debug r30            # Should print '1'
    
    # Print newline
    addi r30, r0, 10
    debug r30
    
    # Print SEQ/SNE results
    add r30, r19, r29
    debug r30            # Should print '1' (5 == 5)
    add r30, r20, r29
    debug r30            # Should print '0' (5 == 10)
    add r30, r21, r29
    debug r30            # Should print '1' (5 != 10)
    add r30, r22, r29
    debug r30            # Should print '0' (5 != 5)
    
    # Print newline
    addi r30, r0, 10
    debug r30
    
    # Print SGT results
    add r30, r23, r29
    debug r30            # Should print '1' (10 > 5)
    add r30, r24, r29
    debug r30            # Should print '0' (5 > 10)
    add r30, r25, r29
    debug r30            # Should print '1' (5 > -5)
    
    # Print newline
    addi r30, r0, 10
    debug r30
    
    # Print SLE results
    add r30, r1, r29
    debug r30            # Should print '1' (5 <= 10)
    add r30, r2, r29
    debug r30            # Should print '1' (5 <= 5)
    add r30, r3, r29
    debug r30            # Should print '0' (10 <= 5)
    
    # Print newline
    addi r30, r0, 10
    debug r30
    
    # Print SGE results
    add r30, r6, r29
    debug r30            # Should print '1' (10 >= 5)
    add r30, r7, r29
    debug r30            # Should print '1' (5 >= 5)
    add r30, r8, r29
    debug r30            # Should print '0' (5 >= 10)
    
    # Print newline
    addi r30, r0, 10
    debug r30
    
    halt