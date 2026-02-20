.text
.global main
main:
    addi r11, r31, 0
    jal r31, abi_call_probe
    addi r2, r0, 123
    bne r1, r2, .Lfail
    addi r31, r11, 0
    addi r1, r0, 0
    jalr r0, r31, 0

.Lfail:
    addi r31, r11, 0
    addi r1, r0, 31
    jalr r0, r31, 0
