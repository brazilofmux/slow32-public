.text
.global main
main:
    addi r19, r31, 0

    addi r11, r0, 11
    addi r12, r0, 12
    addi r13, r0, 13
    addi r14, r0, 14
    addi r15, r0, 15
    addi r16, r0, 16
    addi r17, r0, 17
    addi r18, r0, 18

    addi r3, r0, 10
    addi r4, r0, 3
    addi r5, r0, 8
    addi r6, r0, 2
    jal r31, abi_callee_target

    addi r2, r0, 51
    bne r1, r2, .Lfail_ret

    addi r2, r0, 11
    bne r11, r2, .Lfail_save
    addi r2, r0, 12
    bne r12, r2, .Lfail_save
    addi r2, r0, 13
    bne r13, r2, .Lfail_save
    addi r2, r0, 14
    bne r14, r2, .Lfail_save
    addi r2, r0, 15
    bne r15, r2, .Lfail_save
    addi r2, r0, 16
    bne r16, r2, .Lfail_save
    addi r2, r0, 17
    bne r17, r2, .Lfail_save
    addi r2, r0, 18
    bne r18, r2, .Lfail_save

    addi r31, r19, 0
    addi r1, r0, 0
    jalr r0, r31, 0

.Lfail_ret:
    addi r31, r19, 0
    addi r1, r0, 21
    jalr r0, r31, 0

.Lfail_save:
    addi r31, r19, 0
    addi r1, r0, 22
    jalr r0, r31, 0
