// SLOW-32 Runtime: putchar implementation using inline assembly
// Replaces putchar.s with a C version using inline assembly for the DEBUG instruction

int putchar(int c) {
    // DEBUG instruction outputs the character in the register
    // The inline assembly constraint "r" ensures c is in a register
    __asm__ volatile("debug %0" : : "r"(c));
    return c;  // Return the character as per standard putchar behavior
}