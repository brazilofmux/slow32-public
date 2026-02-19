# Minimal hello world for Stage 03 verification
.text
.global main
main:
    # Print '!' using putchar
    li r3, 33
    jal r31, putchar
    # Print newline
    li r3, 10
    jal r31, putchar
    # Exit success
    li r1, 0
    halt
