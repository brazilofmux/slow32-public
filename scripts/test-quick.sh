#!/bin/bash
# Quick test of the toolchain with a simple program

cat > /tmp/test_slow32.c << 'EOF'
#include <stdio.h>

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    printf("SLOW-32 Quick Test\n");
    printf("5! = %d\n", factorial(5));
    
    int sum = 0;
    for (int i = 1; i <= 10; i++) {
        sum += i;
    }
    printf("Sum 1..10 = %d\n", sum);
    
    return 0;
}
EOF

echo "Testing SLOW-32 toolchain..."
./scripts/compile.sh /tmp/test_slow32.c /tmp/test_slow32.s32x

if [ $? -eq 0 ]; then
    echo ""
    echo "Running test program:"
    echo "===================="
    ./tools/emulator/slow32 /tmp/test_slow32.s32x
    echo "===================="
    echo ""
    echo "Test successful!"
    rm -f /tmp/test_slow32.*
else
    echo "Test failed!"
    exit 1
fi