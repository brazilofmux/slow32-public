# SLOW-32 YIELD instruction stub
# Since inline assembly isn't supported yet, we provide this as a function

    .global yield
    .global halt

yield:
    yield r0, r0, 0
    jalr r0, r31, 0    # Return

halt:
    halt r0, r0, 0
    # No return - halt stops execution