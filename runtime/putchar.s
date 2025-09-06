# SLOW-32 Runtime: putchar implementation
# Takes character in r3, outputs via DEBUG instruction

.global putchar

putchar:
    debug r3        # Output character in r3
    add r1, r3, r0  # Return the character
    jalr r0, lr, 0  # Return
