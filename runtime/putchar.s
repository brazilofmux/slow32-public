# SLOW-32 Runtime: putchar implementation
# Takes character in r1 (first arg per calling convention), outputs via DEBUG instruction

.global putchar

putchar:
    debug r1        # Output character in r1 (first arg)
    # r1 already contains return value
    jalr r0, lr, 0  # Return
