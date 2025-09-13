// SLOW-32: debug_char implementation using inline assembly
// Takes character and outputs via DEBUG instruction

void debug_char(int c) {
    __asm__ __volatile__(
        "debug %0"
        :
        : "r"(c)
    );
}