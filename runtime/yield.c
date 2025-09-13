// SLOW-32: yield and halt implementations using inline assembly

void yield(void) {
    __asm__ __volatile__(
        "yield r0, r0, 0"
    );
}

void halt(void) {
    __asm__ __volatile__(
        "halt r0, r0, 0"
    );
    // No return - halt stops execution
}