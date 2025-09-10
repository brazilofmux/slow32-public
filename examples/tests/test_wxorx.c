// Test W^X protection - try to execute from data segment
void _start() {
    // Put some "code" in a data variable
    volatile unsigned int fake_code = 0x7F000000; // HALT instruction
    
    // Try to jump to it (should fail with W^X)
    void (*func)() = (void(*)())&fake_code;
    func();  // This should trigger W^X violation
}
