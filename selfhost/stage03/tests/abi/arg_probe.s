.text
.global abi_probe10
abi_probe10:
    addi r2, r0, 1
    bne r3, r2, .Lbad
    addi r2, r0, 2
    bne r4, r2, .Lbad
    addi r2, r0, 3
    bne r5, r2, .Lbad
    addi r2, r0, 4
    bne r6, r2, .Lbad
    addi r2, r0, 5
    bne r7, r2, .Lbad
    addi r2, r0, 6
    bne r8, r2, .Lbad
    addi r2, r0, 7
    bne r9, r2, .Lbad
    addi r2, r0, 8
    bne r10, r2, .Lbad

    ldw r2, r29, 0
    addi r3, r0, 9
    bne r2, r3, .Lbad
    ldw r2, r29, 4
    addi r3, r0, 10
    bne r2, r3, .Lbad

    addi r1, r0, 123
    jalr r0, r31, 0

.Lbad:
    addi r1, r0, 77
    jalr r0, r31, 0
