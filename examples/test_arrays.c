// Test arrays with proper element size scaling and storage
int main() {
    char array[4] = {1, 2, 3, 4};
    short shorts[2] = {0x1234, 0x5678};
    
    // Test array access with proper element size scaling
    char first = array[0];  // should use byte access
    char third = array[2];  // should scale by 1 byte
    
    short s1 = shorts[0];   // should use half-word access
    short s2 = shorts[1];   // should scale by 2 bytes
    
    // Test storing to arrays
    array[1] = 42;          // should use byte store
    shorts[0] = 0x9ABC;     // should use half-word store
    
    return (int)first + (int)third + (int)s1 + (int)s2;
}