    .text
    .globl main
    .p2align 2
main:
    # Bias allocator pressure so r7 is more likely to remain uncached.
    addi r1, r0, 1
    addi r2, r0, 2
    addi r3, r0, 3
    addi r4, r0, 4
    addi r5, r0, 5
    addi r6, r0, 6
    addi r7, r0, 0

    # Key trigger: forward conditional branch early in the block.
    # Superblock formation extends past this.
    bne r0, r0, cold_path

loop:
    # Clobber temp early each iteration.
    sltiu r9, r1, 1

    # Loop condition reads r7 before the loop-body write.
    sltiu r10, r7, 200
    beq r10, r0, done

    # Keep cache pressure on r1-r6.
    add r1, r1, r2
    add r1, r1, r3
    add r2, r2, r3
    add r3, r3, r4
    add r4, r4, r5
    add r5, r5, r6
    add r6, r6, r1

    # Pending-write candidate near the back-edge.
    addi r7, r7, 1

    # Back-edge (must be recognized during prescan even after forward branch).
    beq r0, r0, loop

cold_path:
    addi r3, r0, 0x43  # 'C'
    debug r3
    addi r3, r0, 0x4F  # 'O'
    debug r3
    addi r3, r0, 0x4C  # 'L'
    debug r3
    addi r3, r0, 0x44  # 'D'
    debug r3
    addi r3, r0, 10
    debug r3
    addi r1, r0, 0
    halt

done:
    addi r11, r0, 200
    bne r7, r11, fail
    addi r3, r0, 0x4F  # 'O'
    debug r3
    addi r3, r0, 0x4B  # 'K'
    debug r3
    addi r3, r0, 10
    debug r3
    addi r1, r0, 0
    halt

fail:
    addi r3, r0, 0x46  # 'F'
    debug r3
    addi r3, r0, 0x41  # 'A'
    debug r3
    addi r3, r0, 0x49  # 'I'
    debug r3
    addi r3, r0, 0x4C  # 'L'
    debug r3
    addi r3, r0, 10
    debug r3
    addi r1, r0, 0
    halt
