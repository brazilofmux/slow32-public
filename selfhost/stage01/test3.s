# Test 3: Function call, data section, load/store
.text
    # Load address of message string
    la r3, msg
    # Call print function
    jal r31, print
    # Exit
    halt

# print: output null-terminated string at r3
print:
    ldbu r4, r3, 0         # load byte
    beq r4, r0, done       # null terminator?
    mv r5, r3              # save r3
    mv r3, r4              # char to print
    debug r3               # print it
    addi r3, r5, 1         # restore r3 + advance
    j print                # loop
done:
    ret

.data
msg:
    .asciz "Hello from Forth assembler!\n"
