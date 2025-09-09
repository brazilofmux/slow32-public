// Test memory protection
int main() {
    // Try to write to code segment (should be protected)
    int *code_ptr = (int*)0x10;
    *code_ptr = 0xDEADBEEF;  // This should fail
    return 0;
}
