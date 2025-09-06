# SLOW-32: debug_char implementation for C code
# Takes character in r3, outputs via DEBUG instruction

.global debug_char

debug_char:
    debug r3        # Output character in r3
    jalr r0, lr, 0  # Return